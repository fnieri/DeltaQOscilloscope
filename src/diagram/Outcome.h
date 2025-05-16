/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing an outcome O_n in a system
 */
#pragma once

#include "Observable.h"
#include "src/maths/DeltaQRepr.h"
class Outcome : public Observable
{
    DeltaQ calculateObservedDeltaQ(uint64_t, uint64_t) override;

public:
    Outcome(const std::string &name);

    double setNewParameters(int newExp, int newNBins) override;

    DeltaQRepr getObservedDeltaQRepr(std::uint64_t, std::uint64_t);

    DeltaQ getObservedDeltaQ(std::uint64_t, std::uint64_t) override;
};
