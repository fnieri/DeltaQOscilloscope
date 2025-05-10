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

DeltaQ Outcome::calculateObservableDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    auto samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);

    sorted = true;
    DeltaQ deltaQ {getBinWidth(), samplesInRange, nBins};
    std::lock_guard<std::mutex> lock(observedMutex);
    observedDeltaQs[timeLowerBound] = deltaQ;
    if (observedDeltaQs.size() > MAX_DQ) {
        auto earliest = observedDeltaQs.begin();
        observedDeltaQs.erase(earliest);
    }
    triggerManager.evaluate(deltaQ, qta);
    return deltaQ;
}
