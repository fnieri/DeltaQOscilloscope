/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */


#include "System.h"

#include "Outcome.h"
#include "exceptions/ComponentAlreadyExists.cpp"
#define N_OF_BINS 10.0

void System::addComponent(const std::shared_ptr<DiagramComponent>& component) {
    const std::string name = component->getName();
    auto it = components.find(name);
    if (it == components.end()) {
        components[component->getName()] = component;
    } else {
        throw ComponentAlreadyExists("Component with name \"" + name + "\" already exists");
    }
    std::shared_ptr<Outcome> outcomePtr = std::dynamic_pointer_cast<Outcome>(component);
    if (outcomePtr) {
        outcomes.push_back(outcomePtr);
    }
    components[component->getName()] = component;
}

void System::setFirstComponent(const std::string& name) {
    const auto it = components.find(name);
    if (it != components.end()) {
        firstComponent = it->second;
    } else {
        throw std::invalid_argument("Component with name " + name + " not found.");
    }
}

std::vector<std::shared_ptr<DiagramComponent>> System::getAllPlottableComponents() const {
    std::vector<std::shared_ptr<DiagramComponent>> plottableComponents;
    for (const auto& componentMap: components) {
        std::shared_ptr<DiagramComponent> component = componentMap.second;
        if (component->isPlottable()) {
            plottableComponents.push_back(component);
        } 
    }
    return plottableComponents;
}

void System::calculateBinWidth() {
    double max = 0;
    for (const std::shared_ptr<Outcome>& outcome: outcomes) {
        const double outcomeMax = outcome->getMax();
        if (outcomeMax > max) {
            max = outcomeMax;
        }
    }
    binWidth = max / N_OF_BINS;
}

double System::getBinWidth() const {
    return binWidth;
}

DeltaQ System::calculateDeltaQ() {
    calculateBinWidth();
    return firstComponent->calculateDeltaQ(*this, DeltaQ());
}
