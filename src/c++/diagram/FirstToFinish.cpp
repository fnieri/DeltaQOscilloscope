#include "FirstToFinish.h"
#include "DeltaQOperations.cpp"

DeltaQ FirstToFinish::calculateDeltaQ() {
    std::vector<double> resultingCdf;

    double sumAtI;
    double productAtI;
    double resultAtI;

    int binWidth = system.getBinWidth();

    std::vector<DeltaQ> deltaQs;
    deltaQs.reserve(followingComponents.size());

    for (DiagramComponent component: followingComponents) {
        deltaQs.push_back(component.calculateDeltaQ());
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