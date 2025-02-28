#include "Operator.h"
#include "DiagramComponent.h"

Operator::Operator(const std::string name, const std::vector<std::shared_ptr<DiagramComponent>> &children)
    : DiagramComponent(name)
    , children {children}
{
}

Operator::Operator(const std::string name)
    : DiagramComponent(name)
{
}

void Operator::addChildren(std::shared_ptr<DiagramComponent> nextComponent)
{
    children.push_back(nextComponent);
}

std::vector<std::shared_ptr<DiagramComponent>> Operator::getChildren()
{
    return children;
}
