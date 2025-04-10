#pragma once

#include "DiagramComponent.h"
#include "Sample.h"
#include <deque>

class Observable : virtual public DiagramComponent
{
protected:
    std::deque<Sample> samples;
    mutable bool sorted;

public:
    explicit Observable(const std::string &name);

    [[nodiscard]] virtual DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound) = 0;

    void addSample(const Sample &sample);

    std::vector<Sample> getSamplesInRange(std::uint64_t timeLowerBound, std::uint64_t timeUpperBound);
};
