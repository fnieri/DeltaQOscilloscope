#include "FirstToFinish.h"
#include "../maths/DeltaQOperations.h"
#include <iostream>
FirstToFinish::FirstToFinish(const std::string &name)
    : Operator(name)
{
}

FirstToFinish::FirstToFinish(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &nextComponents)
    : Operator(name, nextComponents)
    , DiagramComponent(name)
{
}

DeltaQ FirstToFinish::calculateDeltaQ(const System &system, const DeltaQ &deltaQ)
{
    std::vector<long double> resultingCdf;

    const double binWidth = system.getBinWidth();

    std::vector<DeltaQ> deltaQs;
    deltaQs.reserve(nextComponents.size());

    for (const std::shared_ptr<DiagramComponent> &component : nextComponents) {
        deltaQs.push_back(component->calculateDeltaQ(system, deltaQ));
    }

    const int largestDeltaQSize = chooseLongestDeltaQSize(deltaQs);

    for (std::size_t i = 0; i < largestDeltaQSize; i++) {
        double sumAtI = 0;
        double productAtI = 1;
        for (const DeltaQ &probDeltaQ : deltaQs) {
            const double cdfAtI = probDeltaQ.cdfAt(i);
            sumAtI += cdfAtI;
            productAtI *= cdfAtI;
        }
        double resultAtI = sumAtI - productAtI;
        resultingCdf.push_back(resultAtI);
    }
    return {binWidth, resultingCdf, false};
}

std::string FirstToFinish::toString() const
{
    return "First to finish: " + name + "\n";
}

void FirstToFinish::print(int depth) const
{
    std::cout << std::string(depth * 2, ' ') + "First to finish: " + name + "\n";
    for (auto &component : nextComponents) {
        component->print(depth + 1);
    }
    if (followingComponent)
        followingComponent->print(depth);
}
