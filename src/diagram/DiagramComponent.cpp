/**
 * @file DiagramComponent.cpp
 * @author Francesco Nieri
 * @brief Abstract class representing an outcome diagram component
 * @date 25/10/2024
 */

#include "DiagramComponent.h"
#include <iostream>
DiagramComponent::DiagramComponent(const std::string &name)
    : name {name}
{
    std::cout << "Setting name" << name << "\n";
}

std::string DiagramComponent::getName() const &
{
    return name;
}

void DiagramComponent::setNext(const std::string &probeName, std::shared_ptr<DiagramComponent> next)
{
    std::cout << "Called \n";
    probeNextComponent[probeName] = next;
}
