#pragma once

#include "ConfidenceInterval.h"
#include "DeltaQ.h"
#include "DeltaQRepr.h"
class Snapshot
{
    std::string observableName;
    std::vector<DeltaQRepr> observedDeltaQs;
    std::vector<DeltaQRepr> calculatedDeltaQs;

public:
    void addObservedDeltaQ(std::uint64_t, const DeltaQ &, const std::vector<Bound> &);
    void removeOldestObservedDeltaQ();

    void addCalculatedDeltaQ(std::uint64_t, const DeltaQ &, const std::vector<Bound> &);
    void removeOldestCalculatedDeltaQ();

    std::size_t getObservedSize() const;
    std::size_t getCalculatedSize() const;

    void setName(const std::string &);
    std::string getName() &;

    std::vector<DeltaQRepr> getObservedDeltaQs() const;
    std::vector<DeltaQRepr> getCalculatedDeltaQs() const;
};
