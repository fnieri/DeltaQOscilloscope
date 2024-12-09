/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */
#pragma once

#include "../maths/DeltaQ.h"

#include "DiagramComponent.h"
#include "Event.h"
#include "Operator.h"
#include <memory>
#include <unordered_map>

class Outcome;
class DiagramComponent;
class Event;
class Operator;

class System
{
    std::unordered_map<std::string, std::shared_ptr<Event>> events;
    std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomes;
    std::unordered_map<std::string, std::shared_ptr<Operator>> operators;

    std::shared_ptr<DiagramComponent> firstComponent;
    double binWidth {0};

public:
    System() = default;

    void setFirstComponent(std::shared_ptr<DiagramComponent> component);

    void setOutcomes(std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomesMap);

    void setOperators(std::unordered_map<std::string, std::shared_ptr<Operator>> operatorsMap);

    void setEvents(std::unordered_map<std::string, std::shared_ptr<Event>> eventsMap);

    std::shared_ptr<Outcome> getOutcome(const std::string &outcomeName);

    /**
     * Calculate the resulting DeltaQ for the whole system
     */
    DeltaQ calculateDeltaQ();

    void calculateBinWidth();

    double getBinWidth() const;
};
