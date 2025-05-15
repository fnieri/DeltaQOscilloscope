#include "Probe.h"
#include "../maths/ConfidenceInterval.h"
#include "../maths/DeltaQOperations.h"
#include "DiagramComponent.h"
#include <chrono>
#include <iostream>
#include <memory>
#include <mutex>
#define MAX_DQ 30
Probe::Probe(const std::string &name)
    : DiagramComponent(name)
    , Observable(name)
{
}

Probe::Probe(const std::string &name, std::vector<std::shared_ptr<DiagramComponent>> causalLinks)
    : DiagramComponent(name)
    , Observable(name)
    , causalLinks(causalLinks)
{
}

DeltaQ Probe::calculateCalculatedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound).has_value()) {
        return observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound).value().deltaQ;
    }

    std::vector<DeltaQ> deltaQs;
    for (const auto &component : causalLinks) {
        deltaQs.push_back(component->getObservedDeltaQ(timeLowerBound, timeUpperBound));
    }

    std::lock_guard<std::mutex> lock(calcMutex);

    DeltaQ result = convolveN(deltaQs);
    int calculatedBins = result.getBins();

    if (calculatedBins == calculatedInterval.getBins()) {
        calculatedInterval.setNumBins(calculatedBins);
    }

    calculatedInterval.addDeltaQ(result);
    calculatedSnapshot.addCalculatedDeltaQ(timeLowerBound, result, calculatedInterval.getBounds());

    if (observableSnapshot.getCalculatedSize() > MAX_DQ) {
        observableSnapshot.removeOldestCalculatedDeltaQ();
        if (!recording) {
            calculatedInterval.removeDeltaQ(observableSnapshot.getOldestCalculatedDeltaQ());
        }
    }

    return result;
}

DeltaQ Probe::calculateObservedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::vector<Sample> samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);
    if (samplesInRange.empty()) {
        return DeltaQ();
    }

    DeltaQ deltaQ = {getBinWidth(), samplesInRange, nBins};

    observedInterval.addDeltaQ(deltaQ);

    std::lock_guard<std::mutex> lock(observedMutex);

    observableSnapshot.addObservedDeltaQ(timeLowerBound, deltaQ, observedInterval.getBounds());

    if (observableSnapshot.getObservedSize() > MAX_DQ) {
        if (!recording) {
            observedInterval.removeDeltaQ(observableSnapshot.getOldestObservedDeltaQ());
        }
        observableSnapshot.removeOldestObservedDeltaQ();
    }

    triggerManager.evaluate(deltaQ, qta);
    std::cout << deltaQ.toString() << "\n";
    return deltaQ;
}

DeltaQ Probe::getObservedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (observableSnapshot.getObservedDeltaQAtTime(timeLowerBound).has_value()) {
        return observableSnapshot.getObservedDeltaQAtTime(timeLowerBound).value().deltaQ;
    }
    return calculateObservedDeltaQ(timeLowerBound, timeUpperBound);
}

std::vector<Bound> Probe::getBounds() const
{
    return observedInterval.getBounds();
}

std::vector<Bound> Probe::getObservedBounds() const
{
    return observedInterval.getBounds();
}

std::vector<Bound> Probe::getCalculatedBounds() const
{
    return calculatedInterval.getBounds();
}

double Probe::setNewParameters(int newExp, int newNBins)
{
    std::lock_guard<decltype(paramMutex)> lock(paramMutex);
    nBins = newNBins;
    deltaTExp = newExp;
    if (qta.perc_25 > newExp || qta.perc_50 > newExp || qta.perc_75 > newExp) {
        qta = QTA::create(0, 0, 0, qta.cdfMax);
    }
    maxDelay = DELTA_T_BASE * std::pow(2, deltaTExp) * nBins;

    observedInterval = ConfidenceInterval(nBins);
    return maxDelay;
}

DeltaQRepr Probe::getObservedDeltaQRepr(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    auto deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    if (!deltaQRepr.has_value()) {
        calculateObservedDeltaQ(timeLowerBound, timeUpperBound);
        deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    }
    std::cout << deltaQRepr->deltaQ.toString() << "\n";
    return deltaQRepr.value();
}

DeltaQRepr Probe::getCalculatedDeltaQRepr(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    auto deltaQRepr = observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound);
    if (!deltaQRepr.has_value()) {
        calculateCalculatedDeltaQ(timeLowerBound, timeUpperBound);
        deltaQRepr = observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound);
    }
    return deltaQRepr.value();
}
