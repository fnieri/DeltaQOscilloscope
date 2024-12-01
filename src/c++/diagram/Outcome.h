/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing an outcome O_n in a system
 */
#pragma once

#include "Event.h"
#include "DiagramComponent.h"
#include "../System.h"
#include <memory>
#include <cmath>

class Outcome final : virtual public DiagramComponent {
    System system;
    std::shared_ptr<Event> startEvent;
    std::shared_ptr<Event> endEvent;

public:

    Outcome(Event& startEvent, Event& endEvent, System system);
    std::shared_ptr<Event> getStartEvent();
    std::shared_ptr<Event> getEndEvent();
    
    /**
     * Get the samples associated to an outcome
     * //TODO this is just a prototype, it will need to be adjusted to fit time constraints
     */
    std::vector<double> getOutcomeSamples() const;

    DeltaQ getDeltaQ() const;

    double getMax() const;
};