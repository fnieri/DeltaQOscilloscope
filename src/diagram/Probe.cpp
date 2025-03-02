#include "Probe.h"
#include "DiagramComponent.h"
#include <algorithm>
#include <iostream>
#include <memory>

Probe::Probe(const std::string &name)
    : DiagramComponent(name)
{
}

Probe::Probe(const std::string &name, const std::shared_ptr<DiagramComponent> firstComponent)
    : DiagramComponent(name)
    , firstComponent(firstComponent)
{
}
std::vector<long double> Probe::getTimeSeries() const
{
    std::vector<long double> timeSeries {};

    for (const std::pair<double, double> &sample : samples) {
        double elapsedTime = sample.second - sample.first;
        timeSeries.push_back(elapsedTime);
    }
    return timeSeries;
}

DeltaQ Probe::getDeltaQ(double binWidth) const
{
    std::vector<long double> outcomeSamples = getTimeSeries();
    int nBins = getMax(outcomeSamples) / binWidth;
    return {binWidth, outcomeSamples, nBins};
}

double Probe::getMax() const
{
    std::vector<long double> outcomeSamples = getTimeSeries();
    if (!outcomeSamples.empty()) {
        return *std::max_element(outcomeSamples.begin(), outcomeSamples.end());
    }
    return 0;
}

double Probe::getMax(std::vector<long double> samples) const
{
    if (!samples.empty()) {
        return *std::max_element(samples.begin(), samples.end());
    }
    return 0;
}

void Probe::addSample(std::pair<long long, long long> sample)
{
    samples.push_back(sample);
}

void Probe::setFirstComponent(std::shared_ptr<DiagramComponent> component)
{
    firstComponent = component;
}

std::string Probe::toString() const
{
    return "Probe: " + name + "\n";
}

DeltaQ Probe::calculateDeltaQ(const System &system, const DeltaQ &deltaQ)
{
    return NULL;
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
