
#pragma once
#include "diagram/System.h"
#include "server/ErlangSender.h"
#include <functional>
#include <iostream>
#include <memory>
#include <mutex>
#include <vector>
class Application
{
    std::shared_ptr<System> system = nullptr;
    std::vector<std::function<void()>> observers; // List of functions to notify
    ErlangSender sender;

public:
    static Application &getInstance();
    void setSystem(const System newSystem);
    std::shared_ptr<System> getSystem();
    void addObserver(std::function<void()> callback);
    void sendDelayChange(std::string &, double);

private:
    void notifyObservers();
    Application();
};
