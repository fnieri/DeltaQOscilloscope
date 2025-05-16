#include "ErlangSender.h"
#include <arpa/inet.h>
#include <cstring>
#include <iostream>
#include <sys/socket.h>
#include <unistd.h>

ErlangSender::ErlangSender()
    : sock(-1)
{
    erlang_addr.sin_family = AF_INET;
    erlang_addr.sin_port = htons(8081);

    if (inet_pton(AF_INET, "127.0.0.1", &erlang_addr.sin_addr) <= 0) {
        std::cerr << "Invalid address/Address not supported" << std::endl;
    }
    std::cout << "connected \n";
}

ErlangSender::~ErlangSender()
{
    closeConnection();
}

bool ErlangSender::connectToErlang()
{

    if (sock >= 0) {
        return true; // Already connected
    }

    if ((sock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        std::cerr << "Socket creation error" << std::endl;
        return false;
    }

    if (connect(sock, (struct sockaddr *)&erlang_addr, sizeof(erlang_addr)) < 0) {
        std::cerr << "Connection to Erlang failed" << std::endl;
        closeConnection();
        return false;
    }

    return true;
}

void ErlangSender::sendToErlang(const std::string &message)
{

    if (!connectToErlang()) {
        std::cerr << "Cannot send to Erlang - no connection" << std::endl;
        return;
    }

    std::string msgWithNewline = message + "\n";

    if (send(sock, msgWithNewline.c_str(), msgWithNewline.length(), 0) < 0) {
        std::cerr << "Send failed" << std::endl;
        closeConnection();
    }
}

void ErlangSender::closeConnection()
{
    if (sock >= 0) {
        close(sock);
        sock = -1;
    }
}
