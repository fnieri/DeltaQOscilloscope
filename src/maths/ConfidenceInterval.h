#pragma once

#include "DeltaQ.h"
#include <vector>
// Upper and lower confidence bounds of a DeltaQ's CDF
struct Bound {
    double lowerBound {0};
    double upperBound {1};
    double mean {0};
};

class ConfidenceInterval
{
private:
    std::vector<Bound> bounds;
    unsigned int numBins;
    std::vector<double> cdfSum;
    std::vector<double> cdfSumSquares;
    std::vector<unsigned int> cdfSampleCounts;
    double z {1.96};
    void updateConfidenceInterval();

public:
    ConfidenceInterval(int numBins);

    void setNumBins(int newNumBins);

    void addDeltaQ(const DeltaQ &);
    void removeDeltaQ(const DeltaQ &);

    std::vector<Bound> getBounds() const;

    void reset();
};
