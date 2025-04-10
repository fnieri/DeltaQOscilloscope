#pragma once

#include "DiagramComponent.h"
#include <memory>

class Operator : virtual public DiagramComponent
{
protected:
    std::vector<std::shared_ptr<DiagramComponent>> children;

public:
    Operator(const std::string name, const std::vector<std::shared_ptr<DiagramComponent>> &children);

    Operator(const std::string name);

    void addChildren(std::shared_ptr<DiagramComponent> children);

    std::vector<std::shared_ptr<DiagramComponent>> getChildren();

    virtual DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound) = 0;
};
