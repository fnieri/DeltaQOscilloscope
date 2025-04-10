#include "Outcome.h"
#include "../maths/DeltaQOperations.h"
#include "DiagramComponent.h"
#include "System.h"
#include <algorithm>
#include <iostream>
Outcome::Outcome(const std::string &name)
    : DiagramComponent(name)
    , Observable(name)
{
}
/*
DeltaQ Outcome::calculateDeltaQ(const double &binWidth, std::string currentProbe)
{
    if (probeNextComponent.count(currentProbe)) {
        return convolve(getDeltaQ(binWidth), probeNextComponent.at(currentProbe)->calculateDeltaQ(binWidth, currentProbe));
    }
    return getDeltaQ(binWidth);
}
*/
// TODO implement
DeltaQ Outcome::calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (probeNextComponent.count(currentProbe)) {
        DeltaQ outcomeDeltaQ = DeltaQ();
        if (deltaQs.count(timeLowerBound)) {
            outcomeDeltaQ = deltaQs[timeLowerBound];
        } else {
            outcomeDeltaQ = getDeltaQ(binWidth, timeLowerBound, timeUpperBound);
        }
        return convolve(outcomeDeltaQ, probeNextComponent.at(currentProbe)->calculateDeltaQ(binWidth, currentProbe, timeLowerBound, timeUpperBound));
    }
    return getDeltaQ(binWidth, timeLowerBound, timeUpperBound);
}

DeltaQ Outcome::getDeltaQ(double binWidth, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (deltaQs.count(timeLowerBound)) {
        return deltaQs[timeLowerBound];
    }
    auto samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);

    // FIXME,a lot of samples not getting deleted
    auto it = std::lower_bound(samples.begin(), samples.end(), timeUpperBound, [](const Sample &s, long long time) { return s.startTime < time; });

    samples.erase(samples.begin(), it);
    sorted = true;
    DeltaQ deltaQ {binWidth, samplesInRange};
    deltaQs[timeLowerBound] = deltaQ;
    return deltaQ;
}
/*
DeltaQ Outcome::getDeltaQ(double binWidth) const
{
    std::vector<long double> outcomeSamples = getTimeSeries();
    return {binWidth, outcomeSamples};
}
*/
