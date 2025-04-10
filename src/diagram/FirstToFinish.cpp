#include "FirstToFinish.h"
#include "../maths/DeltaQOperations.h"
#include <iostream>
FirstToFinish::FirstToFinish(const std::string &name)
    : DiagramComponent(name)
    , Operator(name)
{
}

FirstToFinish::FirstToFinish(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &children)
    : Operator(name, children)
    , DiagramComponent(name)
{
}

DeltaQ FirstToFinish::calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    return DeltaQ();
}
/*
DeltaQ FirstToFinish::calculateDeltaQ(const double &binWidth, std::string currentProbe)
{
        std::vector<long double> resultingCdf;

        std::vector<DeltaQ> deltaQs;
        deltaQs.reserve(children.size());

        for (const std::shared_ptr<DiagramComponent> &component : children) {
            deltaQs.push_back(component->calculateDeltaQ(binWidth, deltaQ));
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

    return DeltaQ();
}
*/
