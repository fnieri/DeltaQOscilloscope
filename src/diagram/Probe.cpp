#include "Probe.h"
#include "../maths/ConfidenceInterval.h"
#include "../maths/DeltaQOperations.h"
#include <iostream>
#include <memory>
#include <mutex>
Probe::Probe(const std::string &name)
    : Observable(name)
    , calculatedInterval(0)
{
}

Probe::Probe(const std::string &name, std::vector<std::shared_ptr<Observable>> causalLinks)
    : Observable(name)
    , causalLinks(causalLinks)
    , calculatedInterval(0)
{
}

Probe::~Probe() = default;

DeltaQ Probe::calculateCalculatedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound).has_value()) {
        return observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound).value().deltaQ;
    }
    std::vector<DeltaQ> deltaQs;
    for (const auto &component : causalLinks) {

        deltaQs.push_back(component->getObservedDeltaQ(timeLowerBound, timeUpperBound));
    }

    DeltaQ result = convolveN(deltaQs);
    int calculatedBins = result.getBins();

    if (calculatedBins != calculatedInterval.getBins()) {
        calculatedInterval.setNumBins(calculatedBins);
    }

    calculatedInterval.addDeltaQ(result);
    observableSnapshot.addCalculatedDeltaQ(timeLowerBound, result, calculatedInterval.getBounds());
    if (observableSnapshot.getCalculatedSize() > MAX_DQ) {
        calculatedInterval.removeDeltaQ(observableSnapshot.getOldestCalculatedDeltaQ());
        if (!recording) {
            observableSnapshot.removeOldestCalculatedDeltaQ();
        }
    }

    return result;
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

DeltaQRepr Probe::getCalculatedDeltaQRepr(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::lock_guard<std::mutex> lock(calcMutex);
    auto deltaQRepr = observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound);
    if (!deltaQRepr.has_value()) {
        calculateCalculatedDeltaQ(timeLowerBound, timeUpperBound);
        deltaQRepr = observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound);
    }
    return deltaQRepr.value();
}
