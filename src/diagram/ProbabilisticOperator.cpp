/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing a probabilstic operator in a DeltaQ system
 */

#include "ProbabilisticOperator.h"

#include "../maths/DeltaQOperations.h"
#include <utility>
ProbabilisticOperator::ProbabilisticOperator(const std::string &name)
    : DiagramComponent(name)
    , Operator(name)
{
}
ProbabilisticOperator::ProbabilisticOperator(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &nextComponents)
    : DiagramComponent(name)
    , Operator(name, nextComponents)
{
}

DeltaQ ProbabilisticOperator::calculateDeltaQ(const System &system, const DeltaQ &deltaQ)
{
    std::vector<double> resultingCdf;
    std::vector<DeltaQ> scaledDeltaQs;
    /*
    for (const auto &entry : followingComponentAndProbabilities) {
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
    */
    // TODO Calculate Probabilities
    return {0, resultingCdf, false};
}
