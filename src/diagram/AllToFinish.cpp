#include "AllToFinish.h"
#include "DiagramComponent.h"
#include <iostream>
AllToFinish::AllToFinish(const std::string &name)
    : DiagramComponent(name)
    , Operator(name)
{
}
AllToFinish::AllToFinish(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &children)
    : DiagramComponent(name)
    , Operator(name, children)
{
}

DeltaQ AllToFinish::calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound)

{
    return DeltaQ();
}
/*
DeltaQ AllToFinish::calculateDeltaQ(const double &binWidth, std::string currentProbe)
{
        std::vector<double> resultingCdf;

        std::vector<DeltaQ> deltaQs;
        deltaQs.reserve(children.size());

        for (const std::shared_ptr<DiagramComponent> &component : children) {
            deltaQs.push_back(component->calculateDeltaQ(binWidth, deltaQ));
        }

        DeltaQ result = deltaQs[0];
        for (std::size_t i = 1; i < deltaQs.size(); ++i) {
            result = result * deltaQs[i];
        }
        return result;
    return DeltaQ();
}
*/
