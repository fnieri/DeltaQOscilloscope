/**
 * @file DiagramComponent.h
 * @author Francesco Nieri
 * @brief Abstract class representing an outcome diagram component
 * @date 25/10/2024
 */

#pragma once

#include <map>

#include "../maths/DeltaQ.h"

class DiagramComponent
{

protected:
    std::string name;

    DiagramComponent(const std::string &name);

    virtual DeltaQ calculateObservedDeltaQ(uint64_t, uint64_t) = 0;

public:
    DiagramComponent() = default;

    virtual ~DiagramComponent() = default;

    [[nodiscard]] std::string getName() const &;

    virtual DeltaQ getObservedDeltaQ(uint64_t, uint64_t) = 0;
};
