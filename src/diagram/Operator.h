#pragma once

#include "../maths/DeltaQ.h"
#include "Observable.h"
#include "OperatorType.h"

class Operator : public Observable
{
    OperatorType type;

    std::vector<double> probabilities;

    std::vector<std::vector<std::shared_ptr<Observable>>> causalLinks;

    ConfidenceInterval calculatedInterval;

    std::mutex calcMutex;

    std::deque<DeltaQ> calculatedDeltaQHistory;

public:
    Operator(const std::string &name, OperatorType);

    ~Operator();

    DeltaQ calculateCalculatedDeltaQ(uint64_t, uint64_t);

    DeltaQRepr getCalculatedDeltaQRepr(uint64_t, uint64_t);

    void setProbabilities(const std::vector<double> &);

    std::vector<double> getProbabilities()
    {
        return probabilities;
    }
    std::vector<std::shared_ptr<Observable>> getChildren();

    void setCausalLinks(std::vector<std::vector<std::shared_ptr<Observable>>> links)
    {
        causalLinks = links;
    }

    std::vector<std::vector<std::shared_ptr<Observable>>> getCausalLinks()
    {
        return causalLinks;
    }

    OperatorType getType()
    {
        return type;
    }
};
