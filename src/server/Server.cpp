#include "Server.h"
#include "../Application.h"
#include <arpa/inet.h>
#include <cstdint>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <regex>
#include <signal.h>
#include <sys/socket.h>
#include <unistd.h>

#define TIMEOUT "to"
#define EXEC_OK "ok"
#define FAIL "fa"

// Inspired from https://beej.us/guide/bgnet/html//index.html#client-server-background

/**
 * @brief Constructs the Server and registers system observer.
 * @param port The TCP port to listen on.
 */
Server::Server(int port)
    : port(port)
    , server_fd(0)
    , new_socket(0)
    , server_started(false)
{
    Application::getInstance().addObserver([this]() { this->updateSystem(); });

    // Start worker thread (this can run independently)
    workerThread = std::thread([this]() {
        while (!shutdownWorker) {
            std::unique_lock lock(queueMutex);
            queueCond.wait(lock, [this] { return !sampleQueue.empty() || shutdownWorker; });

            while (!sampleQueue.empty()) {
                auto [name, sample] = sampleQueue.front();
                sampleQueue.pop();
                lock.unlock();

                if (system) {
                    system->addSample(name, sample);
                }

                lock.lock();
            }
        }
    });
}

/**
 * @brief Destructor cleans up sockets and joins threads.
 */
Server::~Server()
{
    std::lock_guard<std::mutex> lock(erlangMutex);
    if (erlang_socket > 0) {
        close(erlang_socket);
        erlang_socket = -1;
    }
    close(new_socket);
    close(server_fd);
    if (serverThread.joinable())
        serverThread.join();
}
/**
 * @brief Updates the system reference from Application.
 */
void Server::updateSystem()
{
    system = Application::getInstance().getSystem();
}

/**
 * @brief Main server loop handling client connections.
 */
void Server::run()
{
    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd == 0) {
        perror("Socket failed");
        return;
    }

    // Configure socket options
    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEPORT, &opt, sizeof(opt));

    // Set large buffer sizes
    int bufSize = 1 << 20; // 1MB
    setsockopt(server_fd, SOL_SOCKET, SO_RCVBUF, &bufSize, sizeof(bufSize));
    setsockopt(server_fd, SOL_SOCKET, SO_SNDBUF, &bufSize, sizeof(bufSize));

    // Bind socket to specified IP
    address.sin_family = AF_INET;
    address.sin_port = htons(port);

    // Parse IP address
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

    // Set non-blocking mode
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
        client_socket = accept(server_fd, (struct sockaddr *)&address, (socklen_t *)&addrlen);

        if (client_socket < 0) {
            if (errno == EWOULDBLOCK || errno == EAGAIN) {
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
                cleanupThreads();
                continue;
            }
            if (!running)
                break; // Server was stopped
            perror("Accept failed");
            continue;
        }

        std::lock_guard<std::mutex> lock(clientsMutex);
        clientThreads.emplace_back(&Server::handleClient, this, client_socket);
    }

    // Cleanup
    close(server_fd);
    server_fd = 0;
    cleanupThreads();
    server_started = false;
}

/**
 * @brief Starts the server on specified IP and port.
 * @param ip The IP address to bind to (default: "0.0.0.0" for all interfaces)
 * @param port The port to listen on
 * @return true if server started successfully
 */
bool Server::startServer(const std::string &ip, int port)
{
    if (server_started) {
        std::cerr << "Server already running. Stop it first." << std::endl;
        return false;
    }

    this->server_ip = ip;
    this->port = port;

    serverThread = std::thread(&Server::run, this);

    // Wait a bit to see if server started successfully
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    return server_started;
}

/**
 * @brief Stops the server and closes all sockets.
 */
void Server::stopServer()
{
    if (!server_started) {
        std::cout << "Server not running." << std::endl;
        return;
    }

    running = false;
    server_started = false;

    // Close server socket to break accept loop
    if (server_fd > 0) {
        close(server_fd);
        server_fd = 0;
    }

    // Join server thread
    if (serverThread.joinable()) {
        serverThread.join();
    }

    // Cleanup client threads
    cleanupThreads();

    std::cout << "Server stopped." << std::endl;
}

/**
 * @brief Sets the Erlang endpoint for connections.
 * @param ip Erlang server IP address
 * @param port Erlang server port
 * @return true if endpoint set successfully
 */
bool Server::setErlangEndpoint(const std::string &ip, int port)
{
    std::lock_guard<std::mutex> lock(erlangMutex);

    // Close existing connection if any
    if (erlang_socket > 0) {
        close(erlang_socket);
        erlang_socket = -1;
    }

    erlang_ip = ip;
    erlang_port = port;

    std::cout << "Erlang endpoint set to " << ip << ":" << port << std::endl;
    return true;
}

/**
 * @brief Connects to the Erlang process.
 * @return true if connection succeeded.
 */
bool Server::connectToErlang()
{
    std::lock_guard<std::mutex> lock(erlangMutex);

    if (erlang_socket > 0)
        return true; // Already connected

    erlang_socket = socket(AF_INET, SOCK_STREAM, 0);
    if (erlang_socket < 0) {
        perror("Erlang socket creation failed");
        return false;
    }

    sockaddr_in erlang_addr {};
    erlang_addr.sin_family = AF_INET;
    erlang_addr.sin_port = htons(erlang_port);

    if (inet_pton(AF_INET, erlang_ip.c_str(), &erlang_addr.sin_addr) <= 0) {
        std::cerr << "Invalid Erlang IP: " << erlang_ip << std::endl;
        close(erlang_socket);
        erlang_socket = -1;
        return false;
    }

    if (connect(erlang_socket, (struct sockaddr *)&erlang_addr, sizeof(erlang_addr)) < 0) {
        perror("Failed to connect to Erlang");
        close(erlang_socket);
        erlang_socket = -1;
        return false;
    }

    std::cout << "Connected to Erlang on " << erlang_ip << ":" << erlang_port << std::endl;
    return true;
}

/**
 * @brief Sends a command to the Erlang process.
 * @param command The command string to send.
 */
void Server::sendToErlang(const std::string &command)
{
    signal(SIGPIPE, SIG_IGN); // Ignore SIGPIPE to prevent crashes on disconnect

    if (!connectToErlang()) {
        std::cerr << "Unable to send to Erlang: not connected.\n";
        return;
    }

    std::lock_guard<std::mutex> lock(erlangMutex);

    std::string msgWithNewline = command + "\n";
    ssize_t sent = send(erlang_socket, msgWithNewline.c_str(), msgWithNewline.size(), 0);

    if (sent == -1) {
        perror("send failed");

        if (errno == EPIPE) {
            std::cerr << "Broken pipe: Erlang side likely disconnected." << std::endl;
            close(erlang_socket);
            erlang_socket = -1;
        }

        return;
    }
    std::cout << "Sent to Erlang: " << command << std::endl;
}

/**
 * @brief Handles communication with a client.
 * @param clientSocket The client socket file descriptor.
 */
void Server::handleClient(int clientSocket)
{
    std::string buffer;
    char tempBuf[4096];

    while (running) {
        int valread = read(clientSocket, tempBuf, sizeof(tempBuf));
        if (valread <= 0) {
            if (valread == 0 || errno == ECONNRESET)
                break;
            if (errno == EWOULDBLOCK || errno == EAGAIN) {
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
                continue;
            }
            perror("Read failed");
            break;
        }

        buffer.append(tempBuf, valread);

        size_t pos;
        size_t offset = 0;
        while ((pos = buffer.find('\n', offset)) != std::string::npos) {
            std::string_view message(buffer.data() + offset, pos - offset);
            parseErlangMessage(message.data(), message.size());
            offset = pos + 1;
        }
        buffer.erase(0, offset);
    }

    close(clientSocket);
}

/**
 * @brief Cleans up finished client threads.
 */
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

/**
 * @brief Stops the server and worker threads.
 */
void Server::stop()
{
    running = false;
    {
        std::lock_guard lock(queueMutex);
        shutdownWorker = true;
    }
    queueCond.notify_all();
    if (workerThread.joinable())
        workerThread.join();
}

/**
 * @brief Parses messages from Erlang and adds samples to queue.
 * @param buffer The message buffer.
 * @param len Length of the message.
 */
void Server::parseErlangMessage(const char *buffer, int len)
{
    if (buffer == nullptr || len <= 0 || len >= 1024) {
        std::cerr << "error" << std::endl;
        return;
    }

    std::string message(buffer, len);

    // Parse message components
    size_t nPos = message.find("n:");
    size_t bPos = message.find(";b:");
    size_t ePos = message.find(";e:");
    size_t sPos = message.find(";s:");

    if (nPos != 0 || bPos == std::string::npos || ePos == std::string::npos || sPos == std::string::npos) {
        std::cerr << "Failed to parse message: " << message << std::endl;
        return;
    }

    // Extract message fields
    std::string name = message.substr(2, bPos - 2);
    std::string bStr = message.substr(bPos + 3, ePos - (bPos + 3));
    std::string eStr = message.substr(ePos + 3, sPos - (ePos + 3));
    std::string statusStr = message.substr(sPos + 3);

    // Convert to sample data
    uint64_t startTime = std::stoull(bStr);
    uint64_t endTime = std::stoull(eStr);
    Sample sample;
    Status status = Status::SUCCESS;

    if (statusStr == TIMEOUT || statusStr == FAIL) {
        status = (statusStr == TIMEOUT) ? Status::TIMEDOUT : Status::FAILED;
    } else if (statusStr != EXEC_OK) {
        std::cerr << "Unknown status: " << statusStr << std::endl;
        return;
    }

    double long elapsed = (endTime - startTime) / 1'000'000'000.0L;
    sample = {startTime, endTime, elapsed, status};

    // Add to processing queue
    {
        std::lock_guard lock(queueMutex);
        sampleQueue.emplace(name, sample);
    }
    queueCond.notify_one();
}
