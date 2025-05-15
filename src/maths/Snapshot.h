#pragma once

#include "ConfidenceInterval.h"
#include "DeltaQ.h"
#include "DeltaQRepr.h"
#include <map>
#include <optional>
class Snapshot
{
    std::string observableName;
    std::map<std::uint64_t, DeltaQRepr> observedDeltaQs;
    std::map<std::uint64_t, DeltaQRepr> calculatedDeltaQs;

public:
    void addObservedDeltaQ(std::uint64_t, const DeltaQ &, const std::vector<Bound> &);
    DeltaQ getOldestObservedDeltaQ() const;
    void removeOldestObservedDeltaQ();

    void addCalculatedDeltaQ(std::uint64_t, const DeltaQ &, const std::vector<Bound> &);
    DeltaQ getOldestCalculatedDeltaQ() const;
    void removeOldestCalculatedDeltaQ();

    std::size_t getObservedSize() const;
    std::size_t getCalculatedSize() const;

    // Call this after exiting a trigger, this will remove the first X entries from observed and calculatedDeltaQs until their size = argument
    void resizeTo(size_t);

    void setName(const std::string &);
    std::string getName() &;

    std::vector<DeltaQRepr> getObservedDeltaQs() const;
    std::vector<DeltaQRepr> getCalculatedDeltaQs() const;

    std::optional<DeltaQRepr> getObservedDeltaQAtTime(std::uint64_t);
    std::optional<DeltaQRepr> getCalculatedDeltaQAtTime(std::uint64_t);
};
