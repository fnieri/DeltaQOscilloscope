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

/**
 * @class Server
 * @brief TCP server for handling client connections and Erlang communication.
 *
 * Erlang connects inbound on a single socket. Span data flows Erlang → C++
 * and commands (start_stub, stop_stub, set_timeout) flow C++ → Erlang on
 * the same socket — no separate outbound connection needed.
 */
class Server
{
public:
    Server(int port);
    ~Server();

    /**
     * @brief Sends a command to Erlang on the active connection.
     * @param command The command string to send (newline appended automatically).
     */
    void sendToErlang(const std::string &command);

    bool startServer(const std::string &ip = "0.0.0.0", int port = 8080);
    void stopServer();
    void stop();

    bool isServerRunning() const { return server_started; }

private:
    void run();
    void handleClient(int clientSocket);
    void cleanupThreads();
    void updateSystem();
    void parseErlangMessage(const char *buffer, int len);

    int server_fd = -1;
    int new_socket = -1;
    struct sockaddr_in address;
    int port;

    std::thread serverThread;
    std::shared_ptr<System> system;

    std::vector<std::thread> clientThreads;
    std::mutex clientsMutex;
    std::atomic<bool> running {false};

    // Socket of the currently connected Erlang client.
    // Set when Erlang connects, cleared when it disconnects.
    // sendToErlang writes on this socket; handleClient reads from it.
    // TCP is full-duplex so concurrent read/write is safe.
    int erlang_socket = -1;
    std::mutex erlangMutex;

    std::queue<std::pair<std::string, Sample>> sampleQueue;
    std::mutex queueMutex;
    std::condition_variable queueCond;
    std::thread workerThread;
    bool shutdownWorker = false;

    std::string server_ip = "0.0.0.0";
    bool server_started = false;
};

#endif
