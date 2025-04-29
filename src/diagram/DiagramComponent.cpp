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
}

std::string DiagramComponent::getName() const &
{
    return name;
}

DeltaQ DiagramComponent::getObservableDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (observedDeltaQs.count(timeLowerBound))
        return observedDeltaQs[timeLowerBound];
    return calculateObservableDeltaQ(timeLowerBound, timeUpperBound);
}
