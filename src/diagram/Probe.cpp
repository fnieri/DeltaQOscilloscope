#include "Probe.h"
#include "../maths/DeltaQOperations.h"
#include "DiagramComponent.h"
#include "src/maths/ConfidenceInterval.h"
#include <iostream>
#include <memory>

#define MAX_DQ 30
Probe::Probe(const std::string &name)
    : DiagramComponent(name)
    , Observable(name)
    , interval(50)
{
}

Probe::Probe(const std::string &name, const std::shared_ptr<DiagramComponent> firstComponent)
    : DiagramComponent(name)
    , Observable(name)
    , firstComponent(firstComponent)
    , interval(50)
{
}

ProbeDeltaQ Probe::getDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::vector<Sample> samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);
    DeltaQ probeDeltaQ;
    double bW = getBinWidth();
    DeltaQ calculatedDeltaQ = this->calculateDeltaQ(getBinWidth(), name, timeLowerBound, timeUpperBound);

    if (samplesInRange.size() > 0) {

        probeDeltaQ = {getBinWidth(), samplesInRange};
        interval.addDeltaQ(probeDeltaQ);
    }
    std::vector<Bound> bounds = interval.getBounds();
    ProbeDeltaQ deltaQ {probeDeltaQ, calculatedDeltaQ, bounds};
    deltaQs[timeLowerBound] = deltaQ;

    if (deltaQs.size() > MAX_DQ) {
        auto earliest = deltaQs.begin();
        interval.removeDeltaQ(earliest->second.probeDeltaQ);
        deltaQs.erase(earliest);
    }
    return deltaQ;
}

void Probe::setFirstComponent(std::shared_ptr<DiagramComponent> component)
{
    firstComponent = component;
}

DeltaQ Probe::calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (currentProbe == name) {
        if (firstComponent)
            return firstComponent->calculateDeltaQ(binWidth, currentProbe, timeLowerBound, timeUpperBound);
    }

    if (firstComponent) {
        DeltaQ probeDeltaQ = firstComponent->calculateDeltaQ(binWidth, name, timeLowerBound, timeUpperBound);
        if (probeNextComponent.at(currentProbe)) {
            return convolve(probeDeltaQ, probeNextComponent.at(currentProbe)->calculateDeltaQ(binWidth, currentProbe, timeLowerBound, timeUpperBound));
        } else {
            return probeDeltaQ;
        }
    }
    return DeltaQ();
}

ProbeDeltaQ Probe::getDeltaQAtTime(uint64_t time)
{
    if (deltaQs.count(time))
        return deltaQs[time];
    return {DeltaQ(), DeltaQ(), interval.getBounds()};
}

ConfidenceInterval Probe::getConfidenceInterval()
{
    return interval;
}
