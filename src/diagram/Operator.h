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
    virtual DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) = 0;
};
