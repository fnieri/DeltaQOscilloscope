/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing an outcome O_n in a system
 */
#pragma once

#include "Observable.h"
#include <map>
class Outcome : public Observable
{
    // timeLowerBound when DeltaQ is created
    std::map<uint64_t, DeltaQ> deltaQs;

public:
    Outcome(const std::string &name);
    [[nodiscard]] DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound) override;

    [[nodiscard]] DeltaQ getDeltaQ(double binWidth, uint64_t timeLowerBound, uint64_t timeUpperBound);
};
