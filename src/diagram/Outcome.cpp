#include "Outcome.h"

#include <algorithm>

Outcome::Outcome(const std::string &name, std::shared_ptr<Event> startEvent, std::shared_ptr<Event> endEvent)
    : DiagramComponent(name)
    , startEvent {std::move(startEvent)}
    , endEvent {std::move(endEvent)}
{
}

std::shared_ptr<Event> Outcome::getStartEvent()
{
    return startEvent;
}

std::shared_ptr<Event> Outcome::getEndEvent()
{
    return endEvent;
}

std::vector<double> Outcome::getOutcomeSamples() const
{

    const std::vector<EventSample> startSamples = startEvent->getSamples();
    const std::vector<EventSample> endSamples = endEvent->getSamples();

    std::vector<double> outcomeSamples;
    std::unordered_map<int, double> endMap;

    for (const EventSample &sample : endSamples) {
        endMap[sample.id] = sample.startTime;
    }
    for (const EventSample &sample : startSamples) {
        auto it = endMap.find(sample.id);
        if (it != endMap.end()) {
            double elapsedTime = it->second - sample.startTime;
            outcomeSamples.push_back(elapsedTime);
        }
    }
    return outcomeSamples;
}

DeltaQ Outcome::getDeltaQ(const System &system) const
{
    std::vector<double> outcomeSamples = getOutcomeSamples();
    return {system.getBinWidth(), outcomeSamples};
}

double Outcome::getMax() const
{
    std::vector<double> outcomeSamples = getOutcomeSamples();
    return *std::max_element(outcomeSamples.begin(), outcomeSamples.end());
}

DeltaQ Outcome::calculateDeltaQ(const System &system, const DeltaQ &deltaQ)
{
    return endEvent->calculateDeltaQ(system, getDeltaQ(system));
}
