/**
 * @file DiagramComponent.h
 * @author Francesco Nieri
 * @brief Abstract class representing an outcome diagram component
 * @date 25/10/2024
 */

#pragma once

#include "../maths/DeltaQ.h"

class System;

class DiagramComponent
{

protected:
    std::string name;
    explicit DiagramComponent(const std::string &name);

public:
    DiagramComponent() = default;
    ~DiagramComponent() { };

    virtual DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) = 0;

    std::string getName() const;

    virtual void print(int depth) const { };
    virtual std::string toString() const = 0;
};
