#include "Operator.h"
#include "DiagramComponent.h"

Operator::Operator(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &nextComponents)
    : DiagramComponent(name)
    , nextComponents {nextComponents}
{
}

Operator::Operator(const std::string &name)
    : DiagramComponent(name)
{
}

void Operator::addNextComponent(std::shared_ptr<DiagramComponent> nextComponent)
{
    nextComponents.push_back(nextComponent);
}

void Operator::setFollowingComponent(std::shared_ptr<DiagramComponent> component)
{
    followingComponent = std::move(component);
}
