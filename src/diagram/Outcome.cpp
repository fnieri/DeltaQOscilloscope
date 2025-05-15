#include "Outcome.h"
#include "DiagramComponent.h"
#include "src/maths/ConfidenceInterval.h"
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

    observableSnapshot.addObservedDeltaQ(timeLowerBound, deltaQ, observedInterval.getBounds());
    if ((observableSnapshot.getObservedSize() > MAX_DQ && !recording)) {
        observableSnapshot.removeOldestObservedDeltaQ();
    }

    return deltaQ;
}

double Observable::setNewParameters(int newExp, int newNBins)
{
    deltaTExp = newExp;
    nBins = newNBins;
    maxDelay = DELTA_T_BASE * std::pow(2, deltaTExp) * nBins;
    observedInterval.setNumBins(newNBins);
    return maxDelay;
}
