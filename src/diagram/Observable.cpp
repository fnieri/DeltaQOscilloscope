#include "Observable.h"
#include "DiagramComponent.h"
#include <algorithm>
#include <cmath>
#include <iostream>
Observable::Observable(const std::string &name)
    : DiagramComponent(name)
{
}
void Observable::addSample(const Sample &sample)
{
    samples.push_back(sample);
    sorted = false;
}

std::vector<Sample> Observable::getSamplesInRange(std::uint64_t lowerTime, std::uint64_t upperTime)
{
    if (!sorted) {
        std::sort(samples.begin(), samples.end(), [](const Sample &a, const Sample &b) { return a.startTime < b.startTime; });
        sorted = true;
    }
    std::vector<Sample> selectedSamples;
    auto lower = std::lower_bound(samples.begin(), samples.end(), lowerTime, [](const Sample &s, long long time) { return s.startTime < time; });

    auto upper = std::upper_bound(samples.begin(), samples.end(), upperTime, [](long long time, const Sample &s) { return time < s.startTime; });

    for (auto it = lower; it != upper; ++it) {
        selectedSamples.push_back(*it);
    }

    return selectedSamples;
}
