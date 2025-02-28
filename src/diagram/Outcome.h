/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing an outcome O_n in a system
 */
#pragma once

#include "DiagramComponent.h"
#include "Probe.h"
#include <cmath>
#include <memory>
class Outcome final : virtual public Probe
{
public:
    Outcome(const std::string &name);

    DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) override;

    [[nodiscard]] DeltaQ getDeltaQ(double binWidth) const override;

    std::string toString() const;

    void print(int depth, std::string currentProbe) override;
};
