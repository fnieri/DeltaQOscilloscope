#include "Application.h"

Application::Application()
{
}
Application &Application::getInstance()
{
    static Application instance;
    return instance;
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

void Application::setServer(Server *s)
{
    server = s;
}

void Application::sendDelayChange(std::string &name, double newDelay)
{
    server->sendToErlang("set_timeout;" + name + ';' + std::to_string(newDelay));
}

void Application::setStubRunning(bool running)
{
    if (running)
        server->sendToErlang("start_stub");
    else
        server->sendToErlang("stop_stub");
}

SystemDiff Application::diffWith(System &newSystem)
{
    SystemDiff diff;

    auto oldSystem = getSystem();
    if (!oldSystem) {
        // No system yet: everything is new
        for (const auto &[name, _] : newSystem.getProbes())
            diff.addedProbes.push_back(name);
        for (const auto &[name, _] : newSystem.getOperators())
            diff.addedOperators.push_back(name);
        for (const auto &[name, _] : newSystem.getOutcomes())
            diff.addedOutcomes.push_back(name);
        return diff;
    }

    auto &oldProbes = oldSystem->getProbes();
    auto &newProbes = newSystem.getProbes();

    for (const auto &[name, probe] : newProbes) {
        if (!oldProbes.count(name)) {
            diff.addedProbes.push_back(name);
        } else if (componentsDiffer(oldProbes.at(name), probe)) {
            diff.changedProbes.push_back(name);
        }
    }
    for (const auto &[name, _] : oldProbes) {
        if (!newProbes.count(name))
            diff.removedProbes.push_back(name);
    }

    auto &oldOps = oldSystem->getOperators();
    auto &newOps = newSystem.getOperators();

    for (const auto &[name, op] : newOps) {
        if (!oldOps.count(name)) {
            diff.addedOperators.push_back(name);
        } else if (componentsDiffer(oldOps.at(name), op)) {
            diff.changedOperators.push_back(name);
        }
    }
    for (const auto &[name, _] : oldOps) {
        if (!newOps.count(name))
            diff.removedOperators.push_back(name);
    }

    auto &oldOut = oldSystem->getOutcomes();
    auto &newOut = newSystem.getOutcomes();

    for (const auto &[name, out] : newOut) {
        if (!oldOut.count(name)) {
            diff.addedOutcomes.push_back(name);
        } else if (componentsDiffer(oldOut.at(name), out)) {
            diff.changedOutcomes.push_back(name);
        }
    }
    for (const auto &[name, _] : oldOut) {
        if (!newOut.count(name))
            diff.removedOutcomes.push_back(name);
    }

    return diff;
}

bool Application::componentsDiffer(const std::shared_ptr<Observable> &a, const std::shared_ptr<Observable> &b)
{
    if (!a || !b || a->getName() != b->getName())
        return true;

    // For Probes, check causalLinks. For Operators, check type, probabilities, children.
    if (auto pa = std::dynamic_pointer_cast<Probe>(a)) {
        auto pb = std::dynamic_pointer_cast<Probe>(b);
        if (!pb)
            return true;

        auto ca = pa->getCausalLinks();
        auto cb = pb->getCausalLinks();

        if (ca.size() != cb.size())
            return true;
        for (size_t i = 0; i < ca.size(); ++i) {
            if (ca[i]->getName() != cb[i]->getName())
                return true;
        }
    }

    if (auto oa = std::dynamic_pointer_cast<Operator>(a)) {
        auto ob = std::dynamic_pointer_cast<Operator>(b);
        if (!ob)
            return true;

        if (oa->getType() != ob->getType())
            return true;

        auto ca = oa->getCausalLinks();
        auto cb = ob->getCausalLinks();

        if (ca.size() != cb.size())
            return true;
        for (size_t i = 0; i < ca.size(); ++i) {
            if (ca[i].size() == cb[i].size()) {
                for (size_t j = 0; j < ca[i].size(); ++j) {
                    if (ca[i][j]->getName() != cb[i][j]->getName())
                        return true;
                }

            } else {
                return true;
            }
        }
        auto probsA = oa->getProbabilities();
        auto probsB = ob->getProbabilities();
        if (probsA != probsB)
            return true;
    }

    return false;
}

void Application::setSystem(System newSystem)
{
    if (!system) {
        system = std::make_shared<System>(newSystem);
        notifyObservers();
        return;
    }
    SystemDiff diff = diffWith(newSystem);

    // Apply removals
    for (const auto &name : diff.removedProbes) {
        system->getProbes().erase(name);
        system->getObservables().erase(name);
    }
    for (const auto &name : diff.removedOperators) {
        system->getOperators().erase(name);
        system->getObservables().erase(name);
    }

    for (const auto &name : diff.removedOutcomes) {
        system->getOutcomes().erase(name);
        system->getObservables().erase(name);
    }

    // Apply changes and additions
    for (const auto &name : diff.changedProbes) {
        system->getProbes()[name]->setCausalLinks(newSystem.getProbes().at(name)->getCausalLinks());
        system->getObservables()[name] = newSystem.getProbes().at(name);
    }
    for (const auto &name : diff.addedProbes) {
        system->getProbes()[name] = newSystem.getProbes().at(name);
        system->getObservables()[name] = newSystem.getProbes().at(name);
    }
    for (const auto &name : diff.changedOperators) {
        system->getOperators()[name] = newSystem.getOperators().at(name);
        system->getObservables()[name] = newSystem.getOperators().at(name);
    }
    for (const auto &name : diff.addedOperators) {
        system->getOperators()[name] = newSystem.getOperators().at(name);
        system->getObservables()[name] = newSystem.getOperators().at(name);
    }

    for (const auto &name : diff.changedOutcomes)
        system->getOutcomes()[name] = newSystem.getOutcomes().at(name);
    for (const auto &name : diff.addedOutcomes) {
        system->getOutcomes()[name] = newSystem.getOutcomes().at(name);
        system->getObservables()[name] = newSystem.getOutcomes().at(name);
    }
    std::string defText = newSystem.getSystemDefinitionText();
    system->setSystemDefinitionText(defText);

    notifyObservers();
}
