#pragma once
#include <string>
#include <vector>

#include "../maths/ConfidenceInterval.h"
#include "../maths/DeltaQ.h"
#include "DiagramComponent.h"
#include "Observable.h"
#include <map>
#include <memory>
#include <mutex>
class Probe : public Observable
{
    std::vector<std::shared_ptr<DiagramComponent>> causalLinks;

    std::mutex paramMutex;
    std::mutex calcMutex;

    std::map<uint64_t, DeltaQ> calculatedDeltaQs;
    ConfidenceInterval calculatedInterval;

    Snapshot calculatedSnapshot;

    DeltaQ calculateObservableDeltaQ(uint64_t, uint64_t) override;

public:
    Probe(const std::string &name);

    Probe(const std::string &name, std::vector<std::shared_ptr<DiagramComponent>>);

    DeltaQ getProbeDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound);

    std::vector<Bound> getBounds() const;

    std::vector<Bound> getObservedBounds() const;

    std::vector<Bound> getCalculatedBounds() const;

    double setNewParameters(int, int) override;

    void setCausalLinks(std::vector<std::shared_ptr<DiagramComponent>> newCausalLinks)
    {
        causalLinks = newCausalLinks;
    }

    std::vector<std::shared_ptr<DiagramComponent>> getCausalLinks()
    {
        return causalLinks;
    }
};
