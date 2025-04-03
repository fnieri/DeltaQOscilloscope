#pragma once

#include "DiagramComponent.h"
#include "Sample.h"
#include <deque>
class Observable : virtual public DiagramComponent
{
    std::deque<Sample> samples;

public:
    explicit Observable(const std::string &name);

    [[nodiscard]] std::vector<long double> getTimeSeries() const;

    virtual DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe) = 0;

    void addSample(const Sample &sample);

    void addSamples(std::vector<Sample> &&samples);

    void addSamples(const std::vector<Sample> &samples);

    [[nodiscard]] double getMax() const;

    [[nodiscard]] double getMax(std::vector<long double> samples) const;

    std::vector<Sample> getSamplesInRange(std::uint64_t lowerTime, std::uint64_t upperTime) const;
};
