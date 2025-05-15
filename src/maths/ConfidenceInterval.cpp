#include "ConfidenceInterval.h"
#include <iostream>
#include <math.h>
#include <stdexcept>
#include <vector>

ConfidenceInterval::ConfidenceInterval()
    : numBins(0)
{
}

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
    const auto &cdf = deltaQ.getCdfValues();

    if (cdf.size() != numBins) {
        std::cerr << "CDF size mismatch in addDeltaQ \n";
        return;
    }

    for (size_t i = 0; i < cdf.size(); ++i) {
        const double cdfValue = cdf[i];

        cdfSum[i] += cdfValue;
        cdfSumSquares[i] += cdfValue * cdfValue;
        cdfSampleCounts[i] += 1;
    }

    updateConfidenceInterval();
}

void ConfidenceInterval::removeDeltaQ(const DeltaQ &deltaQ)
{
    if (deltaQ == DeltaQ()) {
        return;
    }

    const auto &cdf = deltaQ.getCdfValues();

    if (cdf.size() != numBins) {
        return; // Returning a previous DeltaQ which had different bins
    }

    for (size_t i = 0; i < cdf.size(); ++i) {
        const double cdfValue = cdf[i];

        cdfSum[i] -= cdfValue;
        cdfSumSquares[i] -= cdfValue * cdfValue;

        if (cdfSampleCounts[i] == 0) {
            return; // Removing more than needed
        }

        cdfSampleCounts[i] -= 1;
    }

    updateConfidenceInterval();
}

void ConfidenceInterval::updateConfidenceInterval()
{
    for (size_t i = 0; i < bounds.size(); ++i) {
        unsigned int n = cdfSampleCounts[i];
        if (cdfSampleCounts[i] == 0) {
            bounds[i].lowerBound = 0.0;
            bounds[i].upperBound = 0.0;
            continue;
        }

        double mean = cdfSum[i] / n;
        double meanSquare = cdfSumSquares[i] / n;
        double variance = meanSquare - (mean * mean);
        double stddev = std::sqrt(std::max(variance, 0.0));
        double marginOfError = z * stddev / std::sqrt(n);
        bounds[i].lowerBound = std::max(0.0, mean - marginOfError);
        bounds[i].upperBound = std::min(1.0, mean + marginOfError);
        bounds[i].mean = mean;
    }
}

void ConfidenceInterval::reset()
{
    bounds = std::vector<Bound>();
    bounds.resize(numBins);
    cdfSum = std::vector<double>();
    cdfSum.resize(numBins);
    cdfSumSquares = std::vector<double>();
    cdfSumSquares.resize(numBins);
    cdfSampleCounts = std::vector<unsigned int>();
    cdfSampleCounts.resize(numBins);
}

void ConfidenceInterval::setNumBins(int newNumBins)
{
    if (numBins != newNumBins) {
        numBins = newNumBins;
        reset();
    }
}

std::vector<Bound> ConfidenceInterval::getBounds() const
{
    return bounds;
}

unsigned int ConfidenceInterval::getBins()
{
    return numBins;
}
