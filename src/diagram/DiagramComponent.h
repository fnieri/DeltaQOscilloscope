/**
 * @file DiagramComponent.h
 * @author Francesco Nieri
 * @brief Abstract class representing an outcome diagram component
 * @date 25/10/2024
 */

#pragma once

#include "../maths/DeltaQ.h"
#include <memory>
#include <unordered_map>
class System;

class DiagramComponent
{

protected:
    std::string name;
    explicit DiagramComponent(const std::string name);
    // Unordered map indicating in probe x which component is next (by name)
    // probeNextComponent["probe1"]["o2"] indicates that in probe1 said component has o2 as next component (o1 -> o2).
    std::unordered_map<std::string, std::shared_ptr<DiagramComponent>> probeNextComponent;

public:
    DiagramComponent() = default;
    ~DiagramComponent() { };

    void setNext(const std::string &probeName, std::shared_ptr<DiagramComponent> next);

    virtual DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) = 0;

    std::string getName() const;

    virtual void print(int depth, std::string currentProbe) { };
    virtual std::string toString() const = 0;
};
