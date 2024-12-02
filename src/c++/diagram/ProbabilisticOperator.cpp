/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing a probabilstic operator in a DeltaQ system
 */


#include "ProbabilisticOperator.h"

#include <utility>
#include "../maths/DeltaQOperations.h"

ProbabilisticOperator::ProbabilisticOperator(const std::string& name, std::map<std::shared_ptr<DiagramComponent>, double> followingComponents) :
    DiagramComponent{name},
    followingComponentAndProbabilities{std::move(followingComponents)}
    {}  


DeltaQ ProbabilisticOperator::calculateDeltaQ(const System& system, const DeltaQ& deltaQ) {
    std::vector<double> resultingCdf;
    std::vector<DeltaQ> scaledDeltaQs;
    
     for (const auto& entry : followingComponentAndProbabilities) {
        const std::shared_ptr<DiagramComponent> component = entry.first;
        const double probability = entry.second;

        DeltaQ currentDeltaQ = component->calculateDeltaQ(system, deltaQ);

        scaledDeltaQs.push_back(currentDeltaQ * probability);
    }

    DeltaQ result = scaledDeltaQs[0];
    for (std::size_t i = 1; i < scaledDeltaQs.size(); ++i) {
        result = result + scaledDeltaQs[i];
    }
    return result;
} 