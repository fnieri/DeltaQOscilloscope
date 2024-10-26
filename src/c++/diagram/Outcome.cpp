#include "Outcome.h"

std::shared_ptr<Event> Outcome::getStartEvent() {
    return startEvent;
}

std::shared_ptr<Event> Outcome::getEndEvent() {
    return endEvent;
}

std::vector<double> Outcome::getOutcomeSamples() {
    const std::vector<EventSample> startSamples = startEvent->getSamples();
    const std::vector<EventSample> endSamples = endEvent->getSamples();

    std::vector<double> outcomeSamples;
    std::unordered_map<int, double> endMap;

    for (const EventSample& sample : endSamples) {
        endMap[sample.id] = sample.startTime;
    }

    for (const EventSample& sample : startSamples) {
        auto it = endMap.find(sample.id);
        if (it != endMap.end()) {
            double elapsedTime = sample.startTime - it->second;
            outcomeSamples.push_back(elapsedTime);
        }
    }

    return outcomeSamples;
}
