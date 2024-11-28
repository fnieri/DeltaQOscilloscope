#include "FirstToFinish.h"
#include "DeltaQOperations.cpp"

FirstToFinish::FirstToFinish(const std::string& name, 
                const std::vector<std::shared_ptr<DiagramComponent>>& followingComponents)
        : DiagramComponent(name), 
          followingComponents(followingComponents)
          {}


DeltaQ FirstToFinish::calculateDeltaQ(const System& system) {
    std::vector<double> resultingCdf;

    double sumAtI;
    double productAtI;
    double resultAtI;   

    double binWidth = system.getBinWidth();

    std::vector<DeltaQ> deltaQs;
    deltaQs.reserve(followingComponents.size());

    for (std::shared_ptr<DiagramComponent> component: followingComponents) {
        deltaQs.push_back(component->calculateDeltaQ(system));
    }
    
    int largestDeltaQSize = chooseLongestDeltaQSize(deltaQs);

    for (size_t i = 0; i < largestDeltaQSize; i++) {
        sumAtI = 0;
        productAtI = 0;
        for (DeltaQ deltaQ: deltaQs) {
            double cdfAtI = deltaQ.cdfAt(i);
            sumAtI += cdfAtI;
            productAtI *= cdfAtI;
        }
        resultAtI = sumAtI - productAtI;
        resultingCdf.push_back(resultAtI);
    }
    return DeltaQ(binWidth, resultingCdf, false);
}