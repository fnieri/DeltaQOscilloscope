#pragma once
#include <string>
#include <vector>

#include "../maths/DeltaQ.h"
#include "DiagramComponent.h"
class Probe : virtual public DiagramComponent
{
    std::vector<std::pair<long long, long long>> samples;
    std::shared_ptr<DiagramComponent> firstComponent;

public:
    explicit Probe(const std::string &name);

    explicit Probe(const std::string &name, std::shared_ptr<DiagramComponent> firstComponent);
    [[nodiscard]] std::vector<long double> getTimeSeries() const;

    DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) override;

    void setFirstComponent(std::shared_ptr<DiagramComponent> component);

    void addSample(std::pair<long long, long long> sample);

    [[nodiscard]] virtual DeltaQ getDeltaQ(double binWidth) const;

    [[nodiscard]] double getMax() const;

    [[nodiscard]] double getMax(std::vector<long double> samples) const;

    std::string toString() const override;

    void print(int depth, std::string currentProbe) override;
};
