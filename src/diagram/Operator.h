#pragma once

#include "DiagramComponent.h"
#include "OperatorType.h"
#include "../maths/DeltaQ.h"
#include <memory>

class Operator : virtual public DiagramComponent
{
    OperatorType type;
    std::vector<double> probabilities;
    std::vector<std::vector<std::shared_ptr<DiagramComponent>>> causalLinks;

    DeltaQ calculateObservableDeltaQ(uint64_t, uint64_t) override;
public:
    Operator(const std::string name, OperatorType);

    void setProbabilities(const std::vector<double> &);

    std::vector<double> getProbabilities() {
        return probabilities;
    }
    std::vector<std::shared_ptr<DiagramComponent>> getChildren();

    void setCausalLinks(std::vector<std::vector<std::shared_ptr<DiagramComponent>>> links) {
        causalLinks = links;
    }

    std::vector<std::vector<std::shared_ptr<DiagramComponent>>> getCausalLinks() {
        return causalLinks;
    }
    
    OperatorType getType() {
        return type;
    }
};
