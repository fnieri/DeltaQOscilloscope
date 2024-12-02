/**
 * @file DiagramComponent.h
 * @author Francesco Nieri
 * @brief Abstract class representing an outcome diagram component
 * @date 25/10/2024 
 */

#pragma once

#include <string>

#include "EventSample.h"
#include "../maths/DeltaQ.h"
#include "../System.h"

class DiagramComponent {

protected:
    std::string name;
    explicit DiagramComponent(const std::string& name);

public:

    virtual ~DiagramComponent() = default;

    /**
     * Return true only if the component's CDF can be plotted
     */
    virtual bool isPlottable() = 0;

    virtual DeltaQ calculateDeltaQ(const System& system, const DeltaQ& deltaQ) = 0;

    std::string getName();
};
