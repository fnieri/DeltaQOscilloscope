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
 */
class Server
{
public:
    /**
     * @brief Constructs a Server instance.
     * @param port The TCP port to listen on.
     */
    Server(int port);

    /**
     * @brief Destructor cleans up sockets and threads.
     */
    ~Server();

    /**
     * @brief Starts the server and worker threads.
     */
    void start();

    /**
     * @brief Sends a command to the Erlang process.
     * @param command The command string to send.
     */
    void sendToErlang(const std::string &command);

private:
    /**
     * @brief Main server loop running in a separate thread.
     */
    void run();

    int server_fd;                  ///< Server socket file descriptor
    int new_socket;                 ///< Client socket file descriptor
    struct sockaddr_in address;     ///< Server address structure
    int port;                       ///< Listening port number

    std::thread serverThread;       ///< Thread for server operations

    /**
     * @brief Updates the system reference from Application.
     */
    void updateSystem();

    /**
     * @brief Parses messages from Erlang.
     * @param buffer The message buffer.
     * @param len Length of the message.
     */
    void parseErlangMessage(const char *buffer, int len);

    std::shared_ptr<System> system; ///< Reference to the system being monitored

    std::vector<std::thread> clientThreads; ///< Active client handler threads
    std::mutex clientsMutex;        ///< Mutex for client threads access
    std::atomic<bool> running {false}; ///< Server running state flag

    /**
     * @brief Handles communication with a client.
     * @param clientSocket The client socket file descriptor.
     */
    void handleClient(int clientSocket);

    /**
     * @brief Cleans up finished client threads.
     */
    void cleanupThreads();

    /**
     * @brief Stops the server and worker threads.
     */
    void stop();

    int erlang_socket = -1;         ///< Socket for Erlang communication
    std::mutex erlangMutex;         ///< Mutex for Erlang socket operations

    /**
     * @brief Establishes connection to Erlang.
     * @return true if connection succeeded.
     */
    bool connectToErlang();

    int client_socket;              ///< Current client socket

    // Asynchronous sample processing
    std::queue<std::pair<std::string, Sample>> sampleQueue; ///< Sample processing queue
    std::mutex queueMutex;          ///< Mutex for queue access
    std::condition_variable queueCond; ///< Condition variable for queue notifications
    std::thread workerThread;       ///< Worker thread for sample processing
    bool shutdownWorker = false;    ///< Flag to signal worker thread shutdown
};

#endif
