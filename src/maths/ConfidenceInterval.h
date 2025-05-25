#pragma once

#include "DeltaQ.h"
#include <vector>
// Upper and lower confidence bounds of a DeltaQ's CDF
struct Bound {
    double lowerBound {0};
    double upperBound {1};
    double mean {0};
};

/**
 * @class ConfidenceInterval
 * @brief Class representing the confidence interval of a window of DeltaQs
 */
class ConfidenceInterval
{
private:
    std::vector<Bound> bounds; ///< The bounds at each point
    unsigned int numBins;
    unsigned int size {0};
    std::vector<double> cdfSum; ///< The sum of the cdf at each bin
    std::vector<double> cdfSumSquares; ///< Variance at each bin
    std::vector<unsigned int> cdfSampleCounts; ///< Sample per bins
    double z {1}; ///< Confidence interval
    void updateConfidenceInterval();

public:
    /**
     * @brief Default constructor, set to 0 bins
     */
    ConfidenceInterval();
    /**
     * @brief Constructor for ConfidenceInterval with number of bins
     * @param numBins number of bins
     */
    ConfidenceInterval(int numBins);

    void setNumBins(int newNumBins);
    /**
     * @brief Add DeltaQ to intervals
     * @param DeltaQ DeltaQ to add
     */
    void addDeltaQ(const DeltaQ &);
    /**
     * @brief Remove DeltaQ from intervals
     * @param DeltaQ DeltaQ to remove
     */
    void removeDeltaQ(const DeltaQ &);

    /**
     * @brief Get current confidence bounds
     */
    std::vector<Bound> getBounds() const;
    /**
     * @brief Get number of bins
     * @return Number of bins
     */
    unsigned int getBins();
    /**
     * @brief Zero the confidence intervals
     */
    void reset();
};
