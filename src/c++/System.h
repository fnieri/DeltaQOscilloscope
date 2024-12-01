/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */
#pragma once

#include <memory>
#include <string>
#include <unordered_map>

#include "diagram/DiagramComponent.h"


class Outcome;

class System {
private:
    std::unordered_map<std::string, std::shared_ptr<DiagramComponent>> components;
    std::vector<std::shared_ptr<Outcome>> outcomes;
    std::shared_ptr<DiagramComponent> firstComponent;
    double binWidth{0};

public:

    /**
     * Add a component to the list of components of the system
     * @throws ComponentAlreadyExists if a component with the same name exists
     */
    void addComponent(const std::shared_ptr<DiagramComponent>& component);

    /**
     * Set component with the name passed as parameter as first component
     * @throws std::invalid_argument if name does not exist in the list of components
     */
    void setFirstComponent(const std::string& name);
    
    /**
     * Calculate the resulting DeltaQ for the whole system
     * //TODO return an EmpiricalCDF000
     * 
     */
    DeltaQ calculateDeltaQ();

    /**
     * Get all components whose DeltaQ can be plotted
     * Namely, all Outcomes
     */
    std::vector<std::shared_ptr<DiagramComponent>> getAllPlottableComponents();

    void calculateBinWidth();

    double getBinWidth() const;
};
