/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing an outcome O_n in a system
 */
#pragma once

#include "Observable.h"
// TODO remove probe inheritance
class Outcome : public Observable
{
public:
    Outcome(const std::string &name);

    DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe) override;

    [[nodiscard]] DeltaQ getDeltaQ(double binWidth) const;

    std::string toString() const;

    void print(int depth, std::string currentProbe) override;
};
