
#ifndef SERVER_H
#define SERVER_H

#include "../diagram/System.h"
#include <atomic>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <netinet/in.h>
#include <sys/socket.h>
#include <thread>

class Server
{
public:
    Server(int port);
    ~Server();
    void start(); // Start the server in a new thread

private:
    void run(); // The actual server loop
    int server_fd, new_socket;
    struct sockaddr_in address;
    int port;
    std::thread serverThread;
    void updateSystem();
    void parseErlangMessage(const char *buffer, int len);
    std::shared_ptr<System> system;

    std::vector<std::thread> clientThreads;
    std::mutex clientsMutex;
    std::atomic<bool> running {false};

    void handleClient(int clientSocket);
    void cleanupThreads();
    void stop();
};

#endif
