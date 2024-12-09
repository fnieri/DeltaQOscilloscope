/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing an outcome O_n in a system
 */
#pragma once

#include "DiagramComponent.h"
#include "Event.h"
#include "System.h"
#include <cmath>
#include <memory>

class Outcome final : virtual public DiagramComponent
{
    std::shared_ptr<Event> startEvent;
    std::shared_ptr<Event> endEvent;
    std::shared_ptr<DiagramComponent> nextComponent;

public:
    Outcome(const std::string &name, std::shared_ptr<Event> startEvent, std::shared_ptr<Event> endEvent);

    std::shared_ptr<Event> getStartEvent();
    std::shared_ptr<Event> getEndEvent();

    void setNext(std::shared_ptr<DiagramComponent> next);

    // void setNext(std::shared_ptr<DiagramComponent> &&next);
    /**
     *
     * Get the samples associated to an outcome
     * //TODO this is just a prototype, it will need to be adjusted to fit time constraints
     */
    std::vector<double> getOutcomeSamples() const;

    DeltaQ getDeltaQ(const System &system) const;

    DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) override;

    double getMax() const;
};
