#ifndef ERLANG_SENDER_H
#define ERLANG_SENDER_H

#include <mutex>
#include <netinet/in.h>
#include <string>

class ErlangSender
{
public:
    ErlangSender();
    ~ErlangSender();

    bool connectToErlang();
    void sendToErlang(const std::string &message);

private:
    int sock;
    struct sockaddr_in erlang_addr;

    void ensureConnection();
    void closeConnection();
};

#endif // ERLANG_SENDER_H
