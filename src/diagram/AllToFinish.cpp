#include "AllToFinish.h"

AllToFinish::AllToFinish(const std::string &name)
    : DiagramComponent(name)
    , Operator(name)
{
}
AllToFinish::AllToFinish(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &nextComponents)
    : DiagramComponent(name)
    , Operator(name, nextComponents)
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
