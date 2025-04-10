#include "Application.h"
Application::Application()
{
}
Application &Application::getInstance()
{
    static Application instance;
    return instance;
}

void Application::setSystem(const System newSystem)
{
    system = std::make_shared<System>(newSystem);
    notifyObservers();
}

std::shared_ptr<System> Application::getSystem()
{
    return system;
}

void Application::addObserver(std::function<void()> callback)
{
    observers.push_back(callback);
}

void Application::notifyObservers()
{
    for (auto &observer : observers) {
        observer();
    }
}
