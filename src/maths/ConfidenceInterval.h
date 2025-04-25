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
    std::vector<Bound> bounds;
    unsigned int totalSamples;

    std::vector<double> cdfSum;
    std::vector<double> cdfSumSquares;
    std::vector<unsigned int> cdfSampleCounts;

    void updateConfidenceInterval();

public:
    ConfidenceInterval(int numBins, double alpha);

    void addDeltaQ(const DeltaQ &);
    void removeDeltaQ(const DeltaQ &);

    const std::vector<Bound> getBounds() const;
};
