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

std::vector<std::shared_ptr<DiagramComponent>> System::getAllPlottableComponents() {
    std::vector<std::shared_ptr<DiagramComponent>> plottableComponents;
    for (auto componentMap: components) {
        std::shared_ptr<DiagramComponent> component = componentMap.second;
        if (component->isPlottable()) {
            plottableComponents.push_back(component);
        } 
    }
    return plottableComponents;
}

void System::calculateBinWidth() {
    return;
}

double System::getBinWidth() const {
    return binWidth;
}

DeltaQ System::calculateDeltaQ() {
    return firstComponent->calculateDeltaQ();
}