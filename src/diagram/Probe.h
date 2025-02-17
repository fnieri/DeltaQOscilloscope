#pragma once
#include <string>
#include <vector>

#include "../maths/DeltaQ.h"
class Probe
{
    std::string name;
    std::vector<std::pair<long long, long long>> samples;

public:
    Probe(const std::string &name);

    [[nodiscard]] std::vector<long double> getTimeSeries() const;

    void addSample(std::pair<long long, long long> sample);

    [[nodiscard]] virtual DeltaQ getDeltaQ(double binWidth) const;

    [[nodiscard]] double getMax() const;

    [[nodiscard]] double getMax(std::vector<long double> samples) const;
};
