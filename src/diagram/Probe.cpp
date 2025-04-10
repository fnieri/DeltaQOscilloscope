#include "Probe.h"
#include "../maths/DeltaQOperations.h"
#include "DiagramComponent.h"
#include "src/maths/ConfidenceInterval.h"
#include <iostream>
#include <memory>
Probe::Probe(const std::string &name)
    : DiagramComponent(name)
    , Observable(name)
    , interval(50, 0.05) // FIXME
{
}

Probe::Probe(const std::string &name, const std::shared_ptr<DiagramComponent> firstComponent)
    : DiagramComponent(name)
    , Observable(name)
    , firstComponent(firstComponent)
    , interval(50, 0.05) // FIXME
{
}

ProbeDeltaQ Probe::getDeltaQ(double binWidth, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::vector<Sample> samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);
    DeltaQ probeDeltaQ;
    std::vector<Bound> bounds;
    if (samplesInRange.size() > 0) {
        probeDeltaQ = {binWidth, getSamplesInRange(timeLowerBound, timeUpperBound)};
        bounds = interval.addDeltaQ(probeDeltaQ);
    }
    DeltaQ calculatedDeltaQ = this->calculateDeltaQ(binWidth, name, timeLowerBound, timeUpperBound);
    ProbeDeltaQ deltaQ {probeDeltaQ, calculatedDeltaQ, bounds};
    deltaQs[timeLowerBound] = deltaQ;
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

ConfidenceInterval Probe::getConfidenceInterval()
{
    return interval;
}
