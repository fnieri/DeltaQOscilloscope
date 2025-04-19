#pragma once

#include "DiagramComponent.h"
#include "Sample.h"
#include <deque>
#include <math.h>
#define DELTA_T_BASE 0.001

class Observable : virtual public DiagramComponent
{
protected:
    std::deque<Sample> samples;
    mutable bool sorted;

    double maxDelay;
    int deltaTExp; // Exponent for dynamic binning
    int nBins; // Number of bins

public:
    explicit Observable(const std::string &name);

    [[nodiscard]] virtual DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound) = 0;

    void addSample(const Sample &sample);

    std::vector<Sample> getSamplesInRange(std::uint64_t timeLowerBound, std::uint64_t timeUpperBound);

    void setNewParameters(int newExp, int newNBins);

    double getBinWidth() const
    {
        return DELTA_T_BASE * std::pow(2, deltaTExp);
    }
};
