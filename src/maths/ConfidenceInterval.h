#pragma once

#include "DeltaQ.h"
#include <vector>
// Upper and lower confidence bounds of a DeltaQ's CDF
struct Bound {
    double lowerBound {0};
    double upperBound {1};
};

class ConfidenceInterval
{
private:
    std::vector<int> summedCumulativeHistogram;
    std::vector<Bound> bounds;
    double alpha;
    unsigned int totalSamples;

    void addCumulativeHistogram(const std::vector<int> &cumulativeHistogram, unsigned int numSamples);
    void removeCumulativeHistogram(const std::vector<int> &cumulativeHistogram, unsigned int numSamples);

    std::vector<double> calculateECDF() const;
    double getDKWEpsilon() const;
    void updateConfidenceInterval();

public:
    ConfidenceInterval(int numBins, double alpha);

    void addDeltaQ(const DeltaQ &);
    void removeDeltaQ(const DeltaQ &);

    const std::vector<Bound> getBounds() const;
};
