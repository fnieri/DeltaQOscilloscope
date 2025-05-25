
#pragma once
#include "diagram/Observable.h"
#include "diagram/System.h"
#include "server/Server.h"
#include <functional>
#include <iostream>
#include <memory>
#include <mutex>
#include <vector>

struct SystemDiff {
    std::vector<std::string> addedProbes;
    std::vector<std::string> removedProbes;
    std::vector<std::string> changedProbes;

    std::vector<std::string> addedOutcomes;
    std::vector<std::string> removedOutcomes;
    std::vector<std::string> changedOutcomes;

    std::vector<std::string> addedOperators;
    std::vector<std::string> removedOperators;
    std::vector<std::string> changedOperators;
};

class Application
{

    std::shared_ptr<System> system = nullptr;
    std::vector<std::function<void()>> observers; // List of functions to notify
    Server *server = nullptr;
    bool componentsDiffer(const std::shared_ptr<Observable> &a, const std::shared_ptr<Observable> &b);

    SystemDiff diffWith(System &newSystem);

    Application();
    void notifyObservers();

public:
    static Application &getInstance();
    void setServer(Server *);
    void setSystem(System newSystem);
    std::shared_ptr<System> getSystem();
    void addObserver(std::function<void()> callback);
    void sendDelayChange(std::string &, double);

    bool startCppServer(const std::string &&, int);
    void stopCppServer();

    void setErlangEndpoint(const std::string &&, int);
    void setStubRunning(bool running);
};
