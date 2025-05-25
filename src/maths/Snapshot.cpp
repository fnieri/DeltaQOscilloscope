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
    return calculatedDeltaQs.begin()->second.deltaQ;
}

DeltaQ Snapshot::getOldestObservedDeltaQ() const
{
    return observedDeltaQs.begin()->second.deltaQ;
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
    int calculatedSize = getCalculatedSize();

    // Don't shrink if not needed
    if (observedSize <= newSize && calculatedSize <= newSize)
        return;

    if (observedSize > newSize) {
        int toObserved = observedSize - newSize;
        auto endIt = std::next(observedDeltaQs.begin(), toObserved);
        observedDeltaQs.erase(observedDeltaQs.begin(), endIt);
    }

    if (calculatedSize > newSize) {
        int toCalculated = calculatedSize - newSize;
        auto endItC = std::next(calculatedDeltaQs.begin(), toCalculated);
        calculatedDeltaQs.erase(calculatedDeltaQs.begin(), endItC);
    }

    if (QTAs.size() > newSize) {
        int toSize = QTAs.size() - newSize;
        auto endIt = std::next(QTAs.begin(), toSize);
        QTAs.erase(QTAs.begin(), endIt);
    }
}
void Snapshot::addQTA(uint64_t time, const QTA &qta)
{
    QTAs[time] = qta;
}

std::optional<DeltaQRepr> Snapshot::getObservedDeltaQAtTime(std::uint64_t time)
{
    auto it = observedDeltaQs.find(time);
    if (it != observedDeltaQs.end()) {
        return it->second;
    }
    return std::nullopt;
}

std::optional<DeltaQRepr> Snapshot::getCalculatedDeltaQAtTime(std::uint64_t time)
{
    auto it = calculatedDeltaQs.find(time);
    if (it != calculatedDeltaQs.end()) {
        return it->second;
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

std::vector<DeltaQRepr> Snapshot::getObservedDeltaQs() const &
{
    auto obs = std::vector<DeltaQRepr>();
    for (const auto &s : observedDeltaQs)
        obs.emplace_back(s.second);
    return obs;
}

std::vector<DeltaQRepr> Snapshot::getCalculatedDeltaQs() const &
{
    auto calc = std::vector<DeltaQRepr>();
    for (const auto &s : calculatedDeltaQs)
        calc.emplace_back(s.second);
    return calc;
}

std::vector<QTA> Snapshot::getQTAs() const &
{
    auto QTAsVec = std::vector<QTA>();
    for (const auto &q : QTAs) {
        QTAsVec.emplace_back(q.second);
    }
    return QTAsVec;
}

void Snapshot::setName(const std::string &name)
{
    observableName = name;
}

std::string Snapshot::getName() &
{
    return observableName;
}
