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
    DeltaQ calculateObservableDeltaQ(uint64_t, uint64_t) override;
public:
    Outcome(const std::string &name);
};
