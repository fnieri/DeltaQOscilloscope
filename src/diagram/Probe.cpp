#include "Probe.h"
#include "../maths/DeltaQOperations.h"
#include "DiagramComponent.h"
#include <algorithm>
#include <iostream>
#include <memory>
Probe::Probe(const std::string &name)
    : DiagramComponent(name)
    , Primitive(name)
{
}

Probe::Probe(const std::string &name, const std::shared_ptr<DiagramComponent> firstComponent)
    : DiagramComponent(name)
    , Primitive(name)
    , firstComponent(firstComponent)
{
}

ProbeDeltaQ Probe::getDeltaQ(double binWidth)
{
    std::vector<long double> outcomeSamples = getTimeSeries();
    DeltaQ calculatedDeltaQ = this->calculateDeltaQ(binWidth, name);
    double max = getMax(outcomeSamples);
    int nBins = max / binWidth;
    // Put a limit on number of bins, if it turns out that the number of bins goes into the thousands, Qt won't probably handle this without crashing graciously
    // (Not responding basically)
    if (nBins > 100) {
        std::cout << "Reached limit";
        binWidth = max / 100;
        nBins = 100;
    }
    DeltaQ probeDeltaQ = {binWidth, outcomeSamples, nBins};
    return {probeDeltaQ, calculatedDeltaQ};
}

void Probe::setFirstComponent(std::shared_ptr<DiagramComponent> component)
{
    firstComponent = component;
}

std::string Probe::toString() const
{
    return "Probe: " + name + "\n";
}

DeltaQ Probe::calculateDeltaQ(const double &binWidth, std::string currentProbe)
{

    if (currentProbe == name) {
        std::cout << "Calculating for " << name << "\n";
        if (firstComponent)
            return firstComponent->calculateDeltaQ(binWidth, currentProbe);
    }

    if (firstComponent) {
        DeltaQ probeDeltaQ = firstComponent->calculateDeltaQ(binWidth, name);
        if (probeNextComponent.at(currentProbe)) {
            return convolve(probeDeltaQ, probeNextComponent.at(currentProbe)->calculateDeltaQ(binWidth, currentProbe));
        } else {
            return probeDeltaQ;
        }
    }
    return DeltaQ();
}

void Probe::print(int depth, std::string currentProbe)
{
    std::cout << std::string(" ", depth * 2) + "Probe: " + name + "\n";

    if (currentProbe == "system") {
        if (probeNextComponent.count(currentProbe)) {
            probeNextComponent.at(currentProbe)->print(depth, currentProbe);
        }
    } else if (firstComponent)
        firstComponent->print(depth, currentProbe);
}
