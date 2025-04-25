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

// TODO implement
DeltaQ Outcome::calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (probeNextComponent.count(currentProbe)) {
        DeltaQ outcomeDeltaQ = DeltaQ();
        if (deltaQs.count(timeLowerBound)) {
            outcomeDeltaQ = deltaQs[timeLowerBound];
        } else {
            outcomeDeltaQ = getDeltaQ(timeLowerBound, timeUpperBound);
        }
        return convolve(outcomeDeltaQ, probeNextComponent.at(currentProbe)->calculateDeltaQ(binWidth, currentProbe, timeLowerBound, timeUpperBound));
    }
    return getDeltaQ(timeLowerBound, timeUpperBound);
}

DeltaQ Outcome::getDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (deltaQs.count(timeLowerBound)) {
        return deltaQs[timeLowerBound];
    }
    auto samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);

    auto it = std::lower_bound(samples.begin(), samples.end(), timeUpperBound, [](const Sample &s, long long time) { return s.startTime < time; });

    samples.erase(samples.begin(), it);
    sorted = true;
    DeltaQ deltaQ {getBinWidth(), samplesInRange, nBins};
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
