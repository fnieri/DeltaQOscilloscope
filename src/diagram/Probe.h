#pragma once
#include <string>
#include <vector>

#include "../maths/ConfidenceInterval.h"
#include "../maths/DeltaQ.h"
#include "DiagramComponent.h"
#include "Observable.h"
#include <map>
struct ProbeDeltaQ {
    DeltaQ probeDeltaQ;
    DeltaQ calculatedProbeDeltaQ;
    std::vector<Bound> bounds;
};

class Probe : public Observable
{
    std::shared_ptr<DiagramComponent> firstComponent;
    std::map<uint64_t, ProbeDeltaQ> deltaQs;
    ConfidenceInterval interval;

public:
    explicit Probe(const std::string &name);

    explicit Probe(const std::string &name, std::shared_ptr<DiagramComponent> firstComponent);

    [[nodiscard]] DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound) override;

    ProbeDeltaQ getDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound);

    void setFirstComponent(std::shared_ptr<DiagramComponent> component);

    ConfidenceInterval getConfidenceInterval();
};
