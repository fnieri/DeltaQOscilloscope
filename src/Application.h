
#pragma once
#include "diagram/System.h"
#include <functional>
#include <iostream>
#include <memory>
#include <mutex>
#include <vector>
class Application
{
    std::shared_ptr<System> system = nullptr;
    std::vector<std::function<void()>> observers; // List of functions to notify

public:
    static Application &getInstance();
    void setSystem(const System newSystem);
    std::shared_ptr<System> getSystem();
    void addObserver(std::function<void()> callback);

private:
    void notifyObservers();
    Application();
};
