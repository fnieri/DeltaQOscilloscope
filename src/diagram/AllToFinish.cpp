#include "AllToFinish.h"
#include <iostream>
AllToFinish::AllToFinish(const std::string &name)
    : Operator(name)
{
}
AllToFinish::AllToFinish(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &nextComponents)
    : Operator(name, nextComponents)
{
}

DeltaQ AllToFinish::calculateDeltaQ(const System &system, const DeltaQ &deltaQ)
{
    std::vector<double> resultingCdf;

    std::vector<DeltaQ> deltaQs;
    deltaQs.reserve(nextComponents.size());

    for (const std::shared_ptr<DiagramComponent> &component : nextComponents) {
        deltaQs.push_back(component->calculateDeltaQ(system, deltaQ));
    }

    DeltaQ result = deltaQs[0];
    for (std::size_t i = 1; i < deltaQs.size(); ++i) {
        result = result * deltaQs[i];
    }
    return result;
}

std::string AllToFinish::toString() const
{
    return "All to finish: " + name + "\n";
}

void AllToFinish::print(int depth) const
{
    std::cout << std::string(depth * 2, ' ') + "All to finish: " + name + "\n";
    for (auto &component : nextComponents) {
        component->print(depth + 1);
    }
    if (followingComponent)
        followingComponent->print(depth);
}
