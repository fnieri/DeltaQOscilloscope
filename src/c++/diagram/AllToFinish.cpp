#include "AllToFinish.h"
#include "DeltaQOperations.cpp"

DeltaQ AllToFinish::calculateDeltaQ() {
    std::vector<double> resultingCdf;

    std::vector<DeltaQ> deltaQs;
    deltaQs.reserve(followingComponents.size());

    for (DiagramComponent component: followingComponents) {
        deltaQs.push_back(component.calculateDeltaQ());
    }

    DeltaQ result = deltaQs[0];
    for (size_t i = 1; i < deltaQs.size(); ++i) {
        result = result * deltaQs[i];
    }
    return result;
}