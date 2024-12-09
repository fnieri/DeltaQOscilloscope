#pragma once

#include "DiagramComponent.h"
#include <memory>

class Operator : virtual public DiagramComponent
{
protected:
    std::vector<std::shared_ptr<DiagramComponent>> nextComponents;
    std::shared_ptr<DiagramComponent> followingComponent;

public:
    Operator(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &nextComponents);

    Operator(const std::string &name);

    void addNextComponent(std::shared_ptr<DiagramComponent> nextComponent);

    void setFollowingComponent(std::shared_ptr<DiagramComponent> component);

    virtual DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) = 0;
};
