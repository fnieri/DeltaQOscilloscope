/**
 * @file DiagramComponent.cpp
 * @author Francesco Nieri
 * @brief Abstract class representing an outcome diagram component
 * @date 25/10/2024
 */

#include "DiagramComponent.h"

DiagramComponent::DiagramComponent(const std::string name) : 
    name{name} {
}

virtual bool DiagramComponent::isPlottable();

virtual auto DiagramComponent::calculateDeltaQ(); // TODO change auto 

std::string DiagramComponent::getName() {
    return name;
}
