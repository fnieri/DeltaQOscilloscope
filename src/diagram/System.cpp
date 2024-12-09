/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */

#include "System.h"

#include <utility>

#include "Outcome.h"
#define N_OF_BINS 10.0

void System::setFirstComponent(std::shared_ptr<DiagramComponent> component)
{
    firstComponent = std::move(component);
}

void System::setOutcomes(std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomesMap)
{
    outcomes = outcomesMap;
}

void System::setOperators(std::unordered_map<std::string, std::shared_ptr<Operator>> operatorsMap)
{
    operators = operatorsMap;
}

void System::setEvents(std::unordered_map<std::string, std::shared_ptr<Event>> eventsMap)
{
    events = eventsMap;
}

std::shared_ptr<Outcome> System::getOutcome(const std::string &name)
{
    return outcomes[name];
}
void System::calculateBinWidth()
{
    double max = 0;
    for (const auto &[name, outcome] : outcomes) {
        const double outcomeMax = outcome->getMax();
        if (outcomeMax > max) {
            max = outcomeMax;
        }
    }
    binWidth = max / N_OF_BINS;
}

double System::getBinWidth() const
{
    return binWidth;
}

DeltaQ System::calculateDeltaQ()
{
    calculateBinWidth();
    return firstComponent->calculateDeltaQ(*this, DeltaQ());
}
