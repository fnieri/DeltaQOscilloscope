#include "ConfidenceInterval.h"
#include <iostream>
#include <math.h>
#include <stdexcept>

ConfidenceInterval::ConfidenceInterval(int numBins, double alpha)
    : summedCumulativeHistogram(numBins, 0)
    , bounds(numBins)
    , alpha(alpha)
    , totalSamples(0)
{
}
std::vector<Bound> ConfidenceInterval::addDeltaQ(const DeltaQ &deltaQ)
{
    addCumulativeHistogram(deltaQ.getCumulativeHistogram(), deltaQ.getTotalSamples());
    updateConfidenceInterval();
    return getBounds();
}

void ConfidenceInterval::addCumulativeHistogram(const std::vector<int> &cumulative, unsigned int numSamples)
{
    if (cumulative.size() != summedCumulativeHistogram.size()) {
        throw std::invalid_argument("Size mismatch");
    }

    for (size_t i = 0; i < cumulative.size(); ++i) {
        summedCumulativeHistogram[i] += cumulative[i];
    }
    totalSamples += numSamples;
}

std::vector<double> ConfidenceInterval::calculateECDF() const
{
    std::vector<double> ecdf(summedCumulativeHistogram.size());
    for (size_t i = 0; i < summedCumulativeHistogram.size(); ++i) {
        ecdf[i] = static_cast<double>(summedCumulativeHistogram[i]) / totalSamples;
    }
    return ecdf;
}

double ConfidenceInterval::getDKWEpsilon() const
{
    return std::sqrt((1.0 / (2.0 * totalSamples)) * std::log(2.0 / alpha));
}

void ConfidenceInterval::updateConfidenceInterval()
{
    auto ecdf = calculateECDF();
    double epsilon = getDKWEpsilon();
    for (size_t i = 0; i < ecdf.size(); ++i) {
        bounds[i].lowerBound = std::max(0.0, ecdf[i] - epsilon);
        bounds[i].upperBound = std::min(1.0, ecdf[i] + epsilon);
    }
}

const std::vector<Bound> ConfidenceInterval::getBounds() const
{
    return bounds;
}
