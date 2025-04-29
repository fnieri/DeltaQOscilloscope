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
    explicit DiagramComponent(const std::string &name);
    std::map<uint64_t, DeltaQ> observedDeltaQs;
    virtual DeltaQ calculateObservableDeltaQ(uint64_t, uint64_t) = 0;

public:
    DiagramComponent() = default;

    virtual ~DiagramComponent() = default;

    [[nodiscard]] std::string getName() const &;

    DeltaQ getObservableDeltaQ(uint64_t, uint64_t);
};
