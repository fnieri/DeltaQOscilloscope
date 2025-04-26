
#include "Server.h"
#include "../Application.h"
#include <arpa/inet.h>
#include <cstdint>
#include <cstring>
#include <fcntl.h>
#include <iostream>
#include <regex>
#include <unistd.h>

#define TIMEOUT "to"
#define EXEC_OK "ok"
#define FAIL "fa"

Server::Server(int port)
    : port(port)
    , server_fd(0)
    , new_socket(0)
{
    Application::getInstance().addObserver([this]() { this->updateSystem(); });
}

Server::~Server()
{
    close(new_socket);
    close(server_fd);
    if (serverThread.joinable())
        serverThread.join();
}

void Server::start()
{
    serverThread = std::thread(&Server::run, this);

    workerThread = std::thread([this]() {
        while (!shutdownWorker) {
            std::unique_lock lock(queueMutex);
            queueCond.wait(lock, [this] { return !sampleQueue.empty() || shutdownWorker; });

            while (!sampleQueue.empty()) {
                auto [name, sample] = sampleQueue.front();
                sampleQueue.pop();
                lock.unlock(); // Unlock during processing (non-blocking)

                if (system) {
                    system->addSample(name, sample); // Now OK to block if needed
                }

                lock.lock();
            }
        }
    });
}

void Server::updateSystem()
{
    system = Application::getInstance().getSystem();
}

void Server::run()
{
    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd == 0) {
        perror("Socket failed");
        return;
    }

    int opt = 1;
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));
    setsockopt(server_fd, SOL_SOCKET, SO_REUSEPORT, &opt, sizeof(opt));

    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    if (bind(server_fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
        perror("Bind failed");
        return;
    }

    // Set socket to non-blocking
    fcntl(server_fd, F_SETFL, O_NONBLOCK);

    if (listen(server_fd, SOMAXCONN) < 0) {
        perror("Listen failed");
        return;
    }

    std::cout << "Server running on port " << port << std::endl;
    running = true;

    while (running) {
        int addrlen = sizeof(address);
        int client_socket = accept(server_fd, (struct sockaddr *)&address, (socklen_t *)&addrlen);

        if (client_socket < 0) {
            if (errno == EWOULDBLOCK || errno == EAGAIN) {
                // No pending connections, sleep a bit
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
                cleanupThreads();
                continue;
            }
            perror("Accept failed");
            continue;
        }

        std::lock_guard<std::mutex> lock(clientsMutex);
        clientThreads.emplace_back(&Server::handleClient, this, client_socket);
    }

    // Cleanup
    close(server_fd);
    cleanupThreads();
}

void Server::handleClient(int clientSocket)
{
    char buffer[1024] = {0};

    while (running) {
        int valread = read(clientSocket, buffer, sizeof(buffer));
        if (valread <= 0) {
            if (valread == 0 || errno == ECONNRESET) {
                break; // Client disconnected
            }
            if (errno == EWOULDBLOCK || errno == EAGAIN) {
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
                continue;
            }
            perror("Read failed");
            break;
        }

        parseErlangMessage(buffer, valread);
    }

    close(clientSocket);
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

void Server::parseErlangMessage(const char *buffer, int len)
{
    if (buffer == nullptr || len <= 0 || len >= 1024) {
        std::cerr << "Invalid buffer or length" << std::endl;
        return;
    }
    std::string message(buffer, len);

    std::regex pattern(R"(n:(\w+);b:(\d+);e:(\d+);s:(\w+))");
    std::smatch match;

    if (!std::regex_search(message, match, pattern) || match.size() != 5) {
        std::cerr << "Failed to parse message: " << message << std::endl;
        return;
    }

    std::string name = match[1].str();
    uint64_t startTime = std::stoull(match[2].str());
    uint64_t endTime;
    std::string statusStr = match[4].str();
    Sample sample;
    Status status = Status::SUCCESS;

    if (statusStr == TIMEOUT || statusStr == FAIL) {
        status = Status::FAILED;
        if (statusStr == TIMEOUT)
            status = Status::TIMEDOUT;

        sample = {startTime, NULL, NULL, status};
        std::cout << "Received Sample: Name=" << name << ", Start=" << sample.startTime << ", Status=" << status << std::endl;

    } else if (statusStr == EXEC_OK) {
        endTime = std::stoull(match[3].str());
        double long elapsed = (endTime - startTime) / 1'000'000'000.0L;
        Sample sample {startTime, endTime, elapsed, status};

    } else if (statusStr != EXEC_OK) {
        std::cerr << "Unknown status: " << statusStr << std::endl;
        return;
    }

    {
        std::lock_guard lock(queueMutex);
        sampleQueue.emplace(name, sample);
    }
    queueCond.notify_one();
}
