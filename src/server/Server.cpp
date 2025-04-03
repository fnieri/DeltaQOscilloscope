
#include "Server.h"
#include "../Application.h"
#include <arpa/inet.h>
#include <cstdint>
#include <cstring>
#include <iostream>
#include <regex>
#include <unistd.h>

#define TIMEOUT "to"
#define EXEC_OK "ok"

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
}

void Server::updateSystem()
{
    system = Application::getInstance().getSystem();
    std::cout << "updated \n";
}

void Server::run()
{
    server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd == 0) {
        perror("Socket failed");
        return;
    }

    // Allow reuse of the address (fix bind issues)
    int opt = 1;
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0) {
        perror("setsockopt failed");
        return;
    }

    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    if (bind(server_fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
        perror("Bind failed");
        return;
    }
    if (listen(server_fd, 3) < 0) {
        perror("Listen failed");
        return;
    }

    std::cout << "Server running on port " << port << std::endl;

    int addrlen = sizeof(address);
    new_socket = accept(server_fd, (struct sockaddr *)&address, (socklen_t *)&addrlen);
    if (new_socket < 0) {
        perror("Accept failed");
        return;
    }

    char buffer[1024] = {0};

    while (true) {
        int valread = read(new_socket, buffer, sizeof(buffer));
        if (valread <= 0) {
            break;
        }

        parseErlangMessage(buffer, valread);
    }
}
void Server::parseErlangMessage(const char *buffer, int len)
{
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
    if (statusStr == TIMEOUT) {
        status = Status::TIMEDOUT;
        sample = {startTime, NULL, NULL, status};
        std::cout << "Received Sample: Name=" << name << ", Start=" << sample.startTime << ", Status=" << (status == Status::SUCCESS ? "SUCCESS" : "TIMEDOUT")
                  << std::endl;

    } else if (statusStr == EXEC_OK) {
        endTime = std::stoull(match[3].str());
        std::cout << endTime - startTime;
        double long elapsed = (endTime - startTime) / 1'000'000'000.0L;
        Sample sample {startTime, endTime, elapsed, status};
        std::cout << "Received Sample: Name=" << name << ", Start=" << sample.startTime << ", End=" << sample.endTime << ", Elapsed=" << elapsed
                  << ", Status=" << (status == Status::SUCCESS ? "SUCCESS" : "TIMEDOUT") << std::endl;

    } else if (statusStr != EXEC_OK) {
        std::cerr << "Unknown status: " << statusStr << std::endl;
        return;
    }

    system->addSample(name, sample);
}
