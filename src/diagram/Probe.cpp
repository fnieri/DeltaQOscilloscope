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

DeltaQ Probe::getProbeDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{

    std::vector<DeltaQ> deltaQs;
    for (const auto &component : causalLinks) {
        deltaQs.push_back(component->getObservableDeltaQ(timeLowerBound, timeUpperBound));
    }

    std::lock_guard<std::mutex> lock(calcMutex);

    if (calculatedDeltaQs.size() > MAX_DQ) {
        const auto earliest = calculatedDeltaQs.begin();
        calculatedDeltaQs.erase(earliest);
    }

    DeltaQ result = convolveN(deltaQs);
    int calculatedBins = result.getBins();

    if (calculatedBins == calculatedInterval.getBins()) {
        calculatedInterval.setNumBins(calculatedBins);
    }

    calculatedInterval.addDeltaQ(result);

    calculatedSnapshot.addCalculatedDeltaQ(timeLowerBound, result, calculatedInterval.getBounds());

    if ((calculatedSnapshot.getCalculatedSize() > MAX_DQ && !recording)) {
        calculatedSnapshot.removeOldestCalculatedDeltaQ();
    }

    return result;
}

DeltaQ Probe::calculateObservableDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::vector<Sample> samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);
    DeltaQ deltaQ;

    if (!samplesInRange.empty()) {
        deltaQ = {getBinWidth(), samplesInRange, nBins};
        observedInterval.addDeltaQ(deltaQ);
    }
    std::vector<Bound> bounds = observedInterval.getBounds();

    std::lock_guard<std::mutex> lock(observedMutex);
    observedDeltaQs[timeLowerBound] = deltaQ;

    if (observedDeltaQs.size() > MAX_DQ) {
        auto earliest = observedDeltaQs.begin();
        observedInterval.removeDeltaQ(earliest->second);
        observedDeltaQs.erase(earliest);
    }

    triggerManager.evaluate(deltaQ, qta);

    return deltaQ;
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
    return observedInterval.getBounds();
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
