
#pragma once
#include "../diagram/System.h"
#pragma once
#include "../diagram/Sample.h"
#include <functional>
#include <iostream>
#include <memory>
#include <vector>
class Application
{
    std::shared_ptr<System> system = nullptr;
    std::vector<std::function<void()>> observers; // List of functions to notify

public:
    static Application &getInstance()
    {
        static Application instance;
        return instance;
    }

    void setSystem(const System newSystem)
    {
        if (system) {
            system->replaceSystem(newSystem);
        } else {
            system = std::make_shared<System>(newSystem);
        }
        notifyObservers();
    }

    std::shared_ptr<System> getSystem()
    {
        return system;
    }

    void addObserver(std::function<void()> callback)
    {
        observers.push_back(callback);
    }

private:
    void notifyObservers()
    {
        for (auto &observer : observers) {
            observer();
        }
    }

    Application() { };
};
