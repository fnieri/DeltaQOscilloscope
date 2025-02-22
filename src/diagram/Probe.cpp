#include "Probe.h"
#include <algorithm>
#include <iostream>

Probe::Probe(const std::string &name)
    : probeName {name}
{
}

std::vector<long double> Probe::getTimeSeries() const
{
    std::vector<long double> timeSeries;

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

std::string Probe::toString() const
{
    return "Probe: " + probeName + "\n";
}
