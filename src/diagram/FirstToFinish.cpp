#include "FirstToFinish.h"
#include "../maths/DeltaQOperations.h"

FirstToFinish::FirstToFinish(const std::string &name)
    : DiagramComponent(name)
    , Operator(name)
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
        double productAtI = 0;
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
