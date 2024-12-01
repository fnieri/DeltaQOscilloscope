#include "FirstToFinish.h"
#include "../maths/DeltaQOperations.cpp"

FirstToFinish::FirstToFinish(const std::string& name, 
                const std::vector<std::shared_ptr<DiagramComponent>>& followingComponents)
        : DiagramComponent(name), 
          followingComponents(followingComponents)
          {}


DeltaQ FirstToFinish::calculateDeltaQ(const System& system) {
    std::vector<double> resultingCdf;

    const double binWidth = system.getBinWidth();

    std::vector<DeltaQ> deltaQs;
    deltaQs.reserve(followingComponents.size());

    for (const std::shared_ptr<DiagramComponent>& component: followingComponents) {
        deltaQs.push_back(component->calculateDeltaQ(system));
    }

    const int largestDeltaQSize = chooseLongestDeltaQSize(deltaQs);

    for (std::size_t i = 0; i < largestDeltaQSize; i++) {
        double sumAtI = 0;
        double productAtI = 0;
        for (const DeltaQ& deltaQ: deltaQs) {
            const double cdfAtI = deltaQ.cdfAt(i);
            sumAtI += cdfAtI;
            productAtI *= cdfAtI;
        }
        double resultAtI = sumAtI - productAtI;
        resultingCdf.push_back(resultAtI);
    }
    return {binWidth, resultingCdf, false};
}