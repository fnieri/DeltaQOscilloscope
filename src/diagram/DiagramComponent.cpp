/**
 * @file DiagramComponent.cpp
 * @author Francesco Nieri
 * @brief Abstract class representing an outcome diagram component
 * @date 25/10/2024
 */

#include "DiagramComponent.h"
DiagramComponent::DiagramComponent(const std::string &name)
    : name {name}
{
}

std::string DiagramComponent::getName() const &
{
    return name;
}

void DiagramComponent::setNext(const std::string &probeName, std::shared_ptr<DiagramComponent> next)
{
    probeNextComponent[probeName] = next;
}

std::unordered_map<std::string, std::shared_ptr<DiagramComponent>> DiagramComponent::getProbeNextComponent()
{
    return probeNextComponent;
}
