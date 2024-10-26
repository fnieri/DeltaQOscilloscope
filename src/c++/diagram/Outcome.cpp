#include "Outcome.h"

std::unordered_map<int, float> Outcome::getOutcomeSamples() {
    const std::vector<EventSample> startSamples = startEvent->getSamples();
    const std::vector<EventSample> endSamples = endEvent->getSamples();

    std::unordered_map<int, float> outcomeSamples;
    std::unordered_map<int, float> endMap;

    for (const EventSample& sample : endSamples) {
        endMap[sample.id] = sample.startTime;
    }

    for (const EventSample& sample : startSamples) {
        auto it = endMap.find(sample.id);
        if (it != endMap.end()) {
            float elapsedTime = sample.startTime - it->second;
            outcomeSamples[sample.id] = elapsedTime;
        }
    }

    return outcomeSamples;
}

