#include "Probe.h"
#include "../maths/DeltaQOperations.h"
#include "DiagramComponent.h"
#include "../maths/ConfidenceInterval.h"
#include <iostream>
#include <memory>

#define MAX_DQ 30
Probe::Probe(const std::string &name)
    : DiagramComponent(name)
    , Observable(name)
    , interval(50)
{
}

Probe::Probe(const std::string &name, std::vector<std::shared_ptr<DiagramComponent>> causalLinks)
    : DiagramComponent(name)
    , Observable(name)
    , interval(50)
    , causalLinks(causalLinks)
{
}

DeltaQ Probe::getProbeDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::vector<DeltaQ> deltaQs;
    for (const auto &component : causalLinks) {
        deltaQs.push_back(component->getObservableDeltaQ(timeLowerBound, timeUpperBound));
    }

    if (deltaQs.size() > MAX_DQ) {
        const auto earliest = calculatedDeltaQs.begin();
        interval.removeDeltaQ(earliest->second);
        calculatedDeltaQs.erase(earliest);
    }
    return convolveN(deltaQs);
}


DeltaQ Probe::calculateObservableDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::vector<Sample> samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);
    DeltaQ deltaQ;

    if (!samplesInRange.empty()) {
        deltaQ = {getBinWidth(), samplesInRange};
        interval.addDeltaQ(deltaQ);
    }
    std::vector<Bound> bounds = interval.getBounds();
    observedDeltaQs[timeLowerBound] = deltaQ;

    if (observedDeltaQs.size() > MAX_DQ) {
        auto earliest = observedDeltaQs.begin();
        interval.removeDeltaQ(earliest->second);
        observedDeltaQs.erase(earliest);
    }
    triggerManager.evaluate(deltaQ, qta);
    return deltaQ;
}

std::vector<Bound> Probe::getBounds() const
{
    return interval.getBounds();
}

double Probe::setNewParameters(int newExp, int newNBins) {
    Observable::setNewParameters(newExp, newNBins);
    if (qta.perc_25 > newExp || qta.perc_50 > newExp || qta.perc_75 > newExp) {
        qta = QTA::create(0, 0, 0, qta.cdfMax);
    }
    interval.setNumBins(nBins);
    return maxDelay;
}
