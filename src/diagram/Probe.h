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
    std::map<uint64_t, DeltaQ> calculatedDeltaQs;
    ConfidenceInterval interval;
    std::vector<std::shared_ptr<DiagramComponent>> causalLinks;
    DeltaQ calculateObservableDeltaQ(uint64_t, uint64_t) override;
    std::mutex paramMutex;
    std::mutex calcMutex;

public:
    Probe(const std::string &name);

    Probe(const std::string &name, std::vector<std::shared_ptr<DiagramComponent>>);

    DeltaQ getProbeDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound);

    std::vector<Bound> getBounds() const;

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
