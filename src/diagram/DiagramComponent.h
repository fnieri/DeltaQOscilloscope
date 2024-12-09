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
    virtual ~DiagramComponent() = default;

    virtual DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) = 0;

    std::string getName();
};
