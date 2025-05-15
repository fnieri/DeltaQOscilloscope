#include "Snapshot.h"

void Snapshot::addObservedDeltaQ(std::uint64_t time, const DeltaQ &deltaQ, const std::vector<Bound> &bounds)
{
    DeltaQRepr repr = {time, deltaQ, bounds};
    observedDeltaQs.emplace_back(repr);
}

void Snapshot::removeOldestObservedDeltaQ()
{
    if (getObservedSize() == 0)
        return;
    observedDeltaQs.erase(observedDeltaQs.begin());
}

void Snapshot::addCalculatedDeltaQ(std::uint64_t time, const DeltaQ &deltaQ, const std::vector<Bound> &bounds)
{
    DeltaQRepr repr = {time, deltaQ, bounds};
    calculatedDeltaQs.emplace_back(deltaQ);
}

void Snapshot::removeOldestCalculatedDeltaQ()
{
    if (getCalculatedSize() == 0)
        return;

    calculatedDeltaQs.erase(calculatedDeltaQs.begin());
}

std::size_t Snapshot::getObservedSize() const
{
    return observedDeltaQs.size();
}

std::size_t Snapshot::getCalculatedSize() const
{
    return calculatedDeltaQs.size();
}

std::vector<DeltaQRepr> Snapshot::getObservedDeltaQs() const
{
    return observedDeltaQs;
}

std::vector<DeltaQRepr> Snapshot::getCalculatedDeltaQs() const
{
    return calculatedDeltaQs;
}

void Snapshot::setName(const std::string &name)
{
    observableName = name;
}

std::string Snapshot::getName() &
{
    return observableName;
}
