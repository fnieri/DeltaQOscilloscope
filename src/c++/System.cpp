/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */


#include "System.h"
#include "exceptions/ComponentAlreadyExists.cpp"

void System::addComponent(const std::shared_ptr<DiagramComponent>& component) {
    std::string name = component->getName();
    auto it = components.find(name);
    if (it == components.end()) {
        components[component->getName()] = component;
    } else {
        throw ComponentAlreadyExists("Component with name \"" + name + "\" already exists");
    }

    components[component->getName()] = component;
}

void System::setFirstComponent(const std::string& name) {
    auto it = components.find(name);
    if (it != components.end()) {
        firstComponent = it->second;
    } else {
        throw std::invalid_argument("Component with name " + name + " not found.");
    }
}

void System::calculateBinWidth() {
    return;
}

double System::getBinWidth() {
    return binWidth;
}