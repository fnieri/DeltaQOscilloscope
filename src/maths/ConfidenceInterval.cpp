#include "ConfidenceInterval.h"
#include <iostream>
#include <math.h>
#include <stdexcept>
#include <vector>

ConfidenceInterval::ConfidenceInterval(int numBins)

    : numBins(numBins)
    , cdfSum(numBins, 0.0)
    , cdfSumSquares(numBins, 0.0)
    , cdfSampleCounts(numBins, 0)
    , bounds(numBins)
{
}
void ConfidenceInterval::addDeltaQ(const DeltaQ &deltaQ)
{
    const auto& cdf = deltaQ.getCdfValues();

    if (cdf.size() != cdfSum.size()) {
        throw std::invalid_argument("CDF size mismatch in addDeltaQ");
    }

    for (size_t i = 0; i < cdf.size(); ++i) {
        const double cdfValue = cdf[i];

        cdfSum[i] += cdfValue;
        cdfSumSquares[i] += cdfValue * cdfValue;
        cdfSampleCounts[i] += 1; // One more sample for this bin
    }

    updateConfidenceInterval();
}


void ConfidenceInterval::removeDeltaQ(const DeltaQ &deltaQ)
{
    if (deltaQ == DeltaQ()) {
        return;
    }

    const auto& cdf = deltaQ.getCdfValues();

    if (cdf.size() != cdfSum.size()) {
        throw std::invalid_argument("CDF size mismatch in removeDeltaQ");
    }

    for (size_t i = 0; i < cdf.size(); ++i) {
        const double cdfValue = cdf[i];

        cdfSum[i] -= cdfValue;
        cdfSumSquares[i] -= cdfValue * cdfValue;

        if (cdfSampleCounts[i] == 0) {
            throw std::runtime_error("Cannot remove from empty bin");
        }

        cdfSampleCounts[i] -= 1;
    }

    updateConfidenceInterval();
}


void ConfidenceInterval::updateConfidenceInterval()
{
    for (size_t i = 0; i < bounds.size(); ++i) {
        if (cdfSampleCounts[i] == 0) {
            bounds[i].lowerBound = 0.0;
            bounds[i].upperBound = 0.0;
            continue;
        }

        double mean = cdfSum[i] / cdfSampleCounts[i];
        double meanSquare = cdfSumSquares[i] / cdfSampleCounts[i];
        double variance = meanSquare - (mean * mean);
        double stddev = std::sqrt(std::max(variance, 0.0));

        bounds[i].lowerBound = std::max(0.0, mean - stddev);
        bounds[i].upperBound = std::min(1.0, mean + stddev);
        bounds[i].mean = mean;
    }
}


void ConfidenceInterval::reset()
{
    bounds = std::vector<Bound>(numBins);
    cdfSum = std::vector<double>(numBins);
    cdfSumSquares = std::vector<double>(numBins);
    cdfSampleCounts = std::vector<unsigned int>(numBins);
}

void ConfidenceInterval::setNumBins(int newNumBins)
{
    numBins = newNumBins;
    reset(  );
}

std::vector<Bound> ConfidenceInterval::getBounds() const
{
    return bounds;
}
