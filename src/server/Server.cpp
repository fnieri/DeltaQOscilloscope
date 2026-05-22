#include "Server.h"
#include "../Application.h"
#include <arpa/inet.h>
#include <cstdint>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <signal.h>
#include <sys/socket.h>
#include <unistd.h>

#define TIMEOUT "to"
#define EXEC_OK "ok"
#define FAIL    "fa"

// Inspired from https://beej.us/guide/bgnet/html//index.html#client-server-background

Server::Server(int port)
    : port(port)
{
    Application::getInstance().addObserver([this]() { updateSystem(); });

    workerThread = std::thread([this]() {
        while (!shutdownWorker) {
            std::unique_lock<std::mutex> lock(queueMutex);
            queueCond.wait(lock, [this] { return !sampleQueue.empty() || shutdownWorker; });

            while (!sampleQueue.empty()) {
                auto [name, sample] = sampleQueue.front();
                sampleQueue.pop();
                lock.unlock();
                if (system)
                    system->addSample(name, sample);
                lock.lock();
            }
        }
    });
}

Server::~Server()
{
    if (server_fd >= 0) {
        close(server_fd);
        server_fd = -1;
    }
    if (serverThread.joinable())
        serverThread.join();
    if (workerThread.joinable())
        workerThread.join();
}

void Server::updateSystem()
{
    system = Application::getInstance().getSystem();
}

void Server::run()
{
    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
        perror("Socket failed");
        return;
    }

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEPORT, &opt, sizeof(opt));

    int bufSize = 1 << 20;
    setsockopt(server_fd, SOL_SOCKET, SO_RCVBUF, &bufSize, sizeof(bufSize));
    setsockopt(server_fd, SOL_SOCKET, SO_SNDBUF, &bufSize, sizeof(bufSize));

    address.sin_family = AF_INET;
    address.sin_port = htons(port);

    if (server_ip == "0.0.0.0" || server_ip.empty()) {
        address.sin_addr.s_addr = INADDR_ANY;
    } else {
        if (inet_pton(AF_INET, server_ip.c_str(), &address.sin_addr) <= 0) {
            std::cerr << "Invalid IP address: " << server_ip << std::endl;
            close(server_fd);
            return;
        }
    }

    if (bind(server_fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
        perror("Bind failed");
        close(server_fd);
        return;
    }

    fcntl(server_fd, F_SETFL, O_NONBLOCK);

    if (listen(server_fd, SOMAXCONN) < 0) {
        perror("Listen failed");
        close(server_fd);
        return;
    }

    std::cout << "Server running on " << server_ip << ":" << port << std::endl;
    running = true;
    server_started = true;

    while (running) {
        int addrlen = sizeof(address);
        int client_socket = accept(server_fd, (struct sockaddr *)&address, (socklen_t *)&addrlen);

        if (client_socket < 0) {
            if (errno == EWOULDBLOCK || errno == EAGAIN) {
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
                cleanupThreads();
                continue;
            }
            if (!running)
                break;
            perror("Accept failed");
            continue;
        }

        std::lock_guard<std::mutex> lock(clientsMutex);
        clientThreads.emplace_back(&Server::handleClient, this, client_socket);
    }

    if (server_fd >= 0) {
        close(server_fd);
        server_fd = -1;
    }
    cleanupThreads();
    server_started = false;
}

bool Server::startServer(const std::string &ip, int port)
{
    if (server_started) {
        std::cerr << "Server already running. Stop it first." << std::endl;
        return false;
    }

    server_ip = ip;
    this->port = port;
    serverThread = std::thread(&Server::run, this);
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    return server_started;
}

void Server::stopServer()
{
    if (!server_started) {
        std::cout << "Server not running." << std::endl;
        return;
    }

    running = false;
    server_started = false;

    // Close the Erlang client socket so handleClient()'s read() unblocks.
    {
        std::lock_guard<std::mutex> lock(erlangMutex);
        if (erlang_socket >= 0) {
            close(erlang_socket);
            erlang_socket = -1;
        }
    }

    if (server_fd >= 0) {
        close(server_fd);
        server_fd = -1;
    }

    if (serverThread.joinable())
        serverThread.join();

    cleanupThreads();
    std::cout << "Server stopped." << std::endl;
}

void Server::stop()
{
    running = false;
    {
        std::lock_guard<std::mutex> lock(queueMutex);
        shutdownWorker = true;
    }
    queueCond.notify_all();
    if (workerThread.joinable())
        workerThread.join();
}

/**
 * Handles a connected Erlang client. Stores the socket so sendToErlang can
 * write back on it, then reads span messages until the connection closes.
 */
void Server::handleClient(int clientSocket)
{
    {
        int keepalive = 1;
        setsockopt(clientSocket, SOL_SOCKET, SO_KEEPALIVE, &keepalive, sizeof(keepalive));

        std::lock_guard<std::mutex> lock(erlangMutex);
        erlang_socket = clientSocket;
    }

    std::string buffer;
    char tempBuf[4096];

    while (running) {
        int valread = read(clientSocket, tempBuf, sizeof(tempBuf));
        if (valread <= 0) {
            if (valread == 0 || errno == ECONNRESET || errno == EBADF)
                break;
            if (errno == EWOULDBLOCK || errno == EAGAIN) {
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
                continue;
            }
            perror("Read failed");
            break;
        }

        buffer.append(tempBuf, valread);

        size_t offset = 0;
        size_t pos;
        while ((pos = buffer.find('\n', offset)) != std::string::npos) {
            std::string_view message(buffer.data() + offset, pos - offset);
            parseErlangMessage(message.data(), message.size());
            offset = pos + 1;
        }
        buffer.erase(0, offset);
    }

    {
        std::lock_guard<std::mutex> lock(erlangMutex);
        if (erlang_socket >= 0) {
            close(erlang_socket);
            erlang_socket = -1;
        }
        // If erlang_socket was already closed by stopServer(), clientSocket
        // is the same fd and was already closed — don't double-close.
    }
}

/**
 * Sends a command to Erlang on the active inbound connection.
 * No-op with a warning if Erlang is not connected.
 */
void Server::sendToErlang(const std::string &command)
{
    signal(SIGPIPE, SIG_IGN);

    std::lock_guard<std::mutex> lock(erlangMutex);

    if (erlang_socket < 0) {
        std::cerr << "sendToErlang: Erlang is not connected.\n";
        return;
    }

    std::string msg = command + "\n";
    ssize_t sent = send(erlang_socket, msg.c_str(), msg.size(), 0);

    if (sent == -1) {
        perror("sendToErlang: send failed");
        if (errno == EPIPE) {
            std::cerr << "Broken pipe: Erlang disconnected." << std::endl;
            erlang_socket = -1;
        }
    }
}

void Server::cleanupThreads()
{
    std::lock_guard<std::mutex> lock(clientsMutex);
    auto it = clientThreads.begin();
    while (it != clientThreads.end()) {
        if (it->joinable()) {
            it->join();
            it = clientThreads.erase(it);
        } else {
            ++it;
        }
    }
}

void Server::parseErlangMessage(const char *buffer, int len)
{
    if (buffer == nullptr || len <= 0 || len >= 1024) {
        std::cerr << "parseErlangMessage: invalid buffer" << std::endl;
        return;
    }

    std::string message(buffer, len);

    size_t nPos = message.find("n:");
    size_t bPos = message.find(";b:");
    size_t ePos = message.find(";e:");
    size_t sPos = message.find(";s:");

    if (nPos != 0 || bPos == std::string::npos || ePos == std::string::npos || sPos == std::string::npos) {
        if (message != "ping")
            std::cerr << "parseErlangMessage: malformed message: " << message << std::endl;
        return;
    }

    std::string name    = message.substr(2, bPos - 2);
    std::string bStr    = message.substr(bPos + 3, ePos - (bPos + 3));
    std::string eStr    = message.substr(ePos + 3, sPos - (ePos + 3));
    std::string statusStr = message.substr(sPos + 3);

    uint64_t startTime, endTime;
    try {
        startTime = std::stoull(bStr);
        endTime   = std::stoull(eStr);
    } catch (const std::exception &ex) {
        std::cerr << "parseErlangMessage: bad timestamp: " << ex.what() << std::endl;
        return;
    }

    Status status = Status::SUCCESS;
    if (statusStr == TIMEOUT)
        status = Status::TIMEDOUT;
    else if (statusStr == FAIL)
        status = Status::FAILED;
    else if (statusStr != EXEC_OK) {
        std::cerr << "parseErlangMessage: unknown status: " << statusStr << std::endl;
        return;
    }

    long double elapsed = (endTime - startTime) / 1'000'000'000.0L;
    Sample sample = {startTime, endTime, elapsed, status};

    {
        std::lock_guard<std::mutex> lock(queueMutex);
        sampleQueue.emplace(name, sample);
    }
    queueCond.notify_one();
}
