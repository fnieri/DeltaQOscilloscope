#include "AllToFinish.h"
#include "DeltaQOperations.cpp"

AllToFinish::AllToFinish(const std::string& name, 
                const std::vector<std::shared_ptr<DiagramComponent>>& followingComponents)
        : DiagramComponent(name), 
          followingComponents(followingComponents)
          {}

DeltaQ AllToFinish::calculateDeltaQ(const System& system) {
    std::vector<double> resultingCdf;

    std::vector<DeltaQ> deltaQs;
    deltaQs.reserve(followingComponents.size());

    for (const std::shared_ptr<DiagramComponent>& component: followingComponents) {
        deltaQs.push_back(component->calculateDeltaQ(system));
    }

    DeltaQ result = deltaQs[0];
    for (std::size_t i = 1; i < deltaQs.size(); ++i) {
        result = result * deltaQs[i];
    }
    return result;
}