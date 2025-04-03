#include "Observable.h"
#include "DiagramComponent.h"
#include <algorithm>
#include <cmath>
#include <iostream>
Observable::Observable(const std::string &name)
    : DiagramComponent(name)
{
}

std::vector<long double> Observable::getTimeSeries() const
{
    std::vector<long double> timeSeries;
    timeSeries.reserve(samples.size());
    for (const auto &sample : samples) {
        if (std::isfinite(sample.elapsedTime) && sample.elapsedTime >= 0) {
            timeSeries.push_back(sample.elapsedTime);
        } else {
            std::cerr << "Invalid elapsed time: " << sample.elapsedTime << "\n";
        }
    }
    return timeSeries;
}

void Observable::addSamples(std::vector<Sample> &&newSamples)
{
    /*
        samples.reserve(samples.size() + newSamples.size());
        samples.insert(samples.end(), std::make_move_iterator(newSamples.begin()), std::make_move_iterator(newSamples.end()));
    */
}

void Observable::addSamples(const std::vector<Sample> &newSamples)
{
    /*
        samples.reserve(samples.size() + newSamples.size());
        samples.insert(samples.end(), std::make_move_iterator(newSamples.begin()), std::make_move_iterator(newSamples.end()));
    */
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

    std::sort(samples.begin(), samples.end(), [](const Sample &a, const Sample &b) { return a.startTime < b.startTime; });
}

std::vector<Sample> Observable::getSamplesInRange(std::uint64_t lowerTime, std::uint64_t upperTime) const
{
    std::vector<Sample> selectedSamples;

    auto lower = std::lower_bound(samples.begin(), samples.end(), lowerTime, [](const Sample &s, long long time) { return s.startTime < time; });

    auto upper = std::upper_bound(samples.begin(), samples.end(), upperTime, [](long long time, const Sample &s) { return time < s.startTime; });

    for (auto it = lower; it != upper; ++it) {
        selectedSamples.push_back(*it);
    }

    return selectedSamples;
}
