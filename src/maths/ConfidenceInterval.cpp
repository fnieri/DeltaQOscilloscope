#include "ConfidenceInterval.h"
#include <iostream>
#include <math.h>
#include <stdexcept>

ConfidenceInterval::ConfidenceInterval(int numBins, double alpha)
    : cdfSum(numBins, 0.0)
    , cdfSumSquares(numBins, 0.0)
    , cdfSampleCounts(numBins, 0)
    , bounds(numBins)
    , totalSamples(0)
{
}
void ConfidenceInterval::addDeltaQ(const DeltaQ &deltaQ)
{
    // addCumulativeHistogram(deltaQ.getCumulativeHistogram(), deltaQ.getTotalSamples());

    auto cdf = deltaQ.getCdfValues();
    auto numSamples = deltaQ.getTotalSamples();
    if (cdf.size() != cdfSum.size()) {
        throw std::invalid_argument("CDF size mismatch in addDeltaQ");
    }

    for (size_t i = 0; i < cdf.size(); ++i) {
        double cdfValue = cdf[i];
        double weightedCDF = cdfValue * numSamples;

        cdfSum[i] += weightedCDF;
        cdfSumSquares[i] += weightedCDF * cdfValue; // Note: weightedCDF * original cdfValue
        cdfSampleCounts[i] += numSamples;
    }

    totalSamples += numSamples;
    updateConfidenceInterval();
}

void ConfidenceInterval::removeDeltaQ(const DeltaQ &deltaQ)
{
    if (deltaQ == DeltaQ()) {
        return;
    }
    auto cdf = deltaQ.getCdfValues();
    unsigned int numSamples = deltaQ.getTotalSamples();

    if (cdf.size() != cdfSum.size()) {
        throw std::invalid_argument("CDF size mismatch in removeDeltaQ");
    }

    for (size_t i = 0; i < cdf.size(); ++i) {
        double cdfValue = cdf[i];
        double weightedCDF = cdfValue * numSamples;

        cdfSum[i] -= weightedCDF;
        cdfSumSquares[i] -= weightedCDF * cdfValue;

        if (cdfSampleCounts[i] < numSamples) {
            throw std::runtime_error("Removing more CDF samples than stored in bin");
        }

        cdfSampleCounts[i] -= numSamples;
    }

    if (totalSamples < numSamples) {
        throw std::runtime_error("Removing more total samples than stored");
    }

    totalSamples -= numSamples;
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
        double variance = (cdfSumSquares[i] / cdfSampleCounts[i]) - (mean * mean);
        double stddev = std::sqrt(std::max(variance, 0.0));

        bounds[i].lowerBound = std::max(0.0, mean - stddev);
        bounds[i].upperBound = std::min(1.0, mean + stddev);
    }
}

const std::vector<Bound> ConfidenceInterval::getBounds() const
{
    return bounds;
}
