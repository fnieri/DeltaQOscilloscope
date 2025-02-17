
#ifndef SERVER_H
#define SERVER_H

#include "../diagram/System.h"
#include <memory>
#include <netinet/in.h>
#include <sys/socket.h>
#include <thread>
class Server
{
public:
    Server(int port, std::shared_ptr<System> system);
    ~Server();
    void start(); // Start the server in a new thread

private:
    void run(); // The actual server loop
    int server_fd, new_socket;
    struct sockaddr_in address;
    int port;
    std::thread serverThread;

    std::shared_ptr<System> system;
};

#endif
