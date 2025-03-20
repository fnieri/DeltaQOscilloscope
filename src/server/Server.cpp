#include "Server.h"
#include <arpa/inet.h>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <regex>
#include <unistd.h>
Server::Server(int port, std::shared_ptr<System> system)
    : port(port)
    , server_fd(0)
    , new_socket(0)
    , system {system}
{
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
    std::regex pattern(R"((\w+): Start=(-?\d+) End=(-?\d+))");
    std::smatch match;

    while (true) {
        int valread = read(new_socket, buffer, 1024);
        if (valread <= 0)
            break;

        std::string message(buffer, valread);
        memset(buffer, 0, sizeof(buffer));

        if (std::regex_search(message, match, pattern)) {
            std::string workerName = match[1];
            double startTime = std::abs(std::stold(match[2]));
            double endTime = std::abs(std::stold(match[3]));
            Sample sample = {workerName, startTime, endTime};
            system->addSample(workerName, sample);
            //            std::cout << "Received: " << workerName << ", Start=" << std::fixed << std::setprecision(12) << startTime << ", End=" << std::fixed
            //                    << std::setprecision(12) << endTime << std::endl;
        }
    }
}
