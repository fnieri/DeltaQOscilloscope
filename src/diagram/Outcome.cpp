#include "Outcome.h"
#include "../maths/DeltaQOperations.h"
#include "DiagramComponent.h"
#include <algorithm>
#include <iostream>

#define MAX_DQ 30
Outcome::Outcome(const std::string &name)
    : DiagramComponent(name)
    , Observable(name)
{
}


DeltaQ Outcome::calculateObservableDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound) {
    auto samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);

    auto it = std::lower_bound(samples.begin(), samples.end(), timeUpperBound, [](const Sample &s, long long time) { return s.startTime < time; });

    samples.erase(samples.begin(), it);
    sorted = true;
    DeltaQ deltaQ {getBinWidth(), samplesInRange, nBins};
    observedDeltaQs[timeLowerBound] = deltaQ;
    if (observedDeltaQs.size() > MAX_DQ) {
        auto earliest = observedDeltaQs.begin();
        observedDeltaQs.erase(earliest);
    }
    triggerManager.evaluate(deltaQ, qta);
    return deltaQ;
}
