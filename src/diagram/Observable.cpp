#include "Observable.h"
#include "DiagramComponent.h"
#include <algorithm>
#include <iostream>

Observable::Observable(const std::string &name)
    : DiagramComponent(name)
{
}
std::vector<long double> Observable::getTimeSeries() const
{
    std::vector<long double> timeSeries {};

    for (const Sample &sample : samples) {
        double elapsedTime = sample.endTime - sample.startTime;
        timeSeries.push_back(elapsedTime);
    }
    return timeSeries;
}

double Observable::getMax() const
{
    std::vector<long double> timeSeries = getTimeSeries();
    if (!timeSeries.empty()) {
        return *std::max_element(timeSeries.begin(), timeSeries.end());
    }
    return 0;
}

double Observable::getMax(std::vector<long double> samples) const
{
    if (!samples.empty()) {
        return *std::max_element(samples.begin(), samples.end());
    }
    return 0;
}

void Observable::addSample(const Sample &sample)
{
    samples.push_back(sample);
}
