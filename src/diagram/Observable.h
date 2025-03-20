#pragma once

#include "DiagramComponent.h"
#include "Sample.h"
class Observable : virtual public DiagramComponent
{
    std::vector<Sample> samples;

public:
    explicit Observable(const std::string &name);

    [[nodiscard]] std::vector<long double> getTimeSeries() const;

    virtual DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe) = 0;

    void addSample(const Sample &sample);

    [[nodiscard]] double getMax() const;

    [[nodiscard]] double getMax(std::vector<long double> samples) const;
};
