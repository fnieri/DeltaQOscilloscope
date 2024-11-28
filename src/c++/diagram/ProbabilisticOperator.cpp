/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing a probabilstic operator in a DeltaQ system
 */


#include "ProbabilisticOperator.h"
#include "DeltaQOperations.cpp"

ProbabilisticOperator::ProbabilisticOperator(std::string name, std::map<std::shared_ptr<DiagramComponent>, double> followingComponents) :
    DiagramComponent{name},
    followingComponentAndProbabilities{followingComponents}
    {}  


DeltaQ ProbabilisticOperator::calculateDeltaQ() {
    std::vector<double> resultingCdf;
    std::vector<DeltaQ> scaledDeltaQs;
    
     for (const auto& entry : followingComponentAndProbabilities) {
        std::shared_ptr<DiagramComponent> component = entry.first;
        double probability = entry.second;

        DeltaQ currentDeltaQ = component->calculateDeltaQ(system);

        scaledDeltaQs.push_back(currentDeltaQ * probability);
    }

    DeltaQ result = scaledDeltaQs[0];
    for (size_t i = 1; i < scaledDeltaQs.size(); ++i) {
        result = result + scaledDeltaQs[i];
    }
    return result;
} 