#include "Outcome.h"
#include "DiagramComponent.h"
#include "src/maths/ConfidenceInterval.h"
#include "src/maths/DeltaQRepr.h"
#include <iostream>

#define MAX_DQ 30

Outcome::Outcome(const std::string &name)
    : DiagramComponent(name)
    , Observable(name)
{
}

DeltaQRepr Outcome::getObservedDeltaQRepr(std::uint64_t timeLowerBound, std::uint64_t timeUpperBound)
{
    auto deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    if (!deltaQRepr.has_value()) {
        calculateObservedDeltaQ(timeLowerBound, timeUpperBound);
        deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    }
    return deltaQRepr.value();
}

DeltaQ Outcome::getObservedDeltaQ(std::uint64_t timeLowerBound, std::uint64_t timeUpperBound)
{
    auto deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    if (!deltaQRepr.has_value()) {
        calculateObservedDeltaQ(timeLowerBound, timeUpperBound);
        deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    }
    return deltaQRepr.value().deltaQ;
}

DeltaQ Outcome::calculateObservedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (observableSnapshot.getObservedDeltaQAtTime(timeLowerBound).has_value()) {
        return observableSnapshot.getObservedDeltaQAtTime(timeLowerBound).value().deltaQ;
    }

    auto samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);
    if (samplesInRange.empty()) {
        observableSnapshot.addObservedDeltaQ(timeLowerBound, DeltaQ(), observedInterval.getBounds());
        return DeltaQ();
    }

    DeltaQ deltaQ {getBinWidth(), samplesInRange, nBins};

    std::lock_guard<std::mutex> lock(observedMutex);

    triggerManager.evaluate(deltaQ, qta, timeLowerBound);

    observableSnapshot.addObservedDeltaQ(timeLowerBound, deltaQ, observedInterval.getBounds());
    if ((observableSnapshot.getObservedSize() > MAX_DQ && !recording)) {
        observableSnapshot.removeOldestObservedDeltaQ();
    }

    return deltaQ;
}

double Outcome::setNewParameters(int newExp, int newNBins)
{
    deltaTExp = newExp;
    nBins = newNBins;
    maxDelay = DELTA_T_BASE * std::pow(2, deltaTExp) * nBins;
    observedInterval.setNumBins(newNBins);
    return maxDelay;
}
