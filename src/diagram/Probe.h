#pragma once
#include <string>
#include <vector>

#include "../maths/ConfidenceInterval.h"
#include "../maths/DeltaQ.h"
#include "Observable.h"
#include <map>
#include <memory>
#include <mutex>

class Probe : public Observable
{
    std::vector<std::shared_ptr<Observable>> causalLinks;

    std::mutex calcMutex;

    ConfidenceInterval calculatedInterval;

public:
    Probe(const std::string &name);

    Probe(const std::string &name, std::vector<std::shared_ptr<Observable>>);

    ~Probe();

    DeltaQ calculateCalculatedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound);

    DeltaQRepr getCalculatedDeltaQRepr(uint64_t, uint64_t);

    std::vector<Bound> getBounds() const;

    std::vector<Bound> getObservedBounds() const;

    std::vector<Bound> getCalculatedBounds() const;

    void setCausalLinks(std::vector<std::shared_ptr<Observable>> newCausalLinks)
    {
        causalLinks = newCausalLinks;
    }

    std::vector<std::shared_ptr<Observable>> getCausalLinks()
    {
        return causalLinks;
    }
};
