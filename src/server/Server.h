
#ifndef SERVER_H
#define SERVER_H

#include "../diagram/System.h"
#include <atomic>
#include <condition_variable>
#include <memory>
#include <mutex>
#include <netinet/in.h>
#include <queue>
#include <sys/socket.h>
#include <thread>
class Server
{
public:
    Server(int port);
    ~Server();
    void start();

private:
    void run();

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

    // For adding samples in a non blocking way
    std::queue<std::pair<std::string, Sample>> sampleQueue;
    std::mutex queueMutex;
    std::condition_variable queueCond;
    std::thread workerThread;
    bool shutdownWorker = false;
};

#endif
