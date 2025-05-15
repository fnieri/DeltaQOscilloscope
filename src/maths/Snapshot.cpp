#include "Snapshot.h"

void Snapshot::addObservedDeltaQ(std::uint64_t time, const DeltaQ &deltaQ, const std::vector<Bound> &bounds)
{
    DeltaQRepr repr = {time, deltaQ, bounds};
    observedDeltaQs[time] = repr;
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
    calculatedDeltaQs[time] = repr;
}

DeltaQ Snapshot::getOldestCalculatedDeltaQ() const
{
    return calculatedDeltaQs.begin()->first;
}

DeltaQ Snapshot::getOldestObservedDeltaQ() const
{
    return observedDeltaQs.begin()->first;
}

void Snapshot::removeOldestCalculatedDeltaQ()
{
    if (getCalculatedSize() == 0)
        return;

    calculatedDeltaQs.erase(calculatedDeltaQs.begin());
}

void Snapshot::resizeTo(size_t newSize)
{
    int observedSize = getObservedSize();

    if (observedSize < newSize) {
        return;
    }

    int calculatedSize = getCalculatedSize();

    int toObserved = observedSize - newSize;
    int toCalculated = calculatedSize - newSize;

    auto endIt = std::next(observedDeltaQs.begin(), toObserved);
    observedDeltaQs.erase(observedDeltaQs.begin(), endIt);

    auto endItC = std::next(calculatedDeltaQs.begin(), toCalculated);
    calculatedDeltaQs.erase(calculatedDeltaQs.begin(), endItC);
}

std::optional<DeltaQRepr> Snapshot::getObservedDeltaQAtTime(std::uint64_t time)
{
    if (observedDeltaQs.count(time)) {
        return observedDeltaQs[time];
    }
    return std::nullopt;
}

std::optional<DeltaQRepr> Snapshot::getCalculatedDeltaQAtTime(std::uint64_t time)
{
    if (calculatedDeltaQs.count(time)) {
        return calculatedDeltaQs[time];
    }
    return std::nullopt;
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
    auto obs = std::vector<DeltaQRepr>();
    for (const auto &s : observedDeltaQs)
        obs.push_back(s.second);
    return obs;
}

std::vector<DeltaQRepr> Snapshot::getCalculatedDeltaQs() const
{
    auto calc = std::vector<DeltaQRepr>();
    for (const auto &s : calculatedDeltaQs)
        calc.push_back(s.second);
    return calc;
}

void Snapshot::setName(const std::string &name)
{
    observableName = name;
}

std::string Snapshot::getName() &
{
    return observableName;
}
