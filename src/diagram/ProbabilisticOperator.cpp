/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing a probabilstic operator in a DeltaQ system
 */

#include "ProbabilisticOperator.h"

#include "../maths/DeltaQOperations.h"
#include <iostream>
#include <utility>
ProbabilisticOperator::ProbabilisticOperator(const std::string &name)
    : DiagramComponent(name)
    , Operator(name)
{
}
ProbabilisticOperator::ProbabilisticOperator(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &children)
    : DiagramComponent(name)
    , Operator(name, children)
{
}

DeltaQ ProbabilisticOperator::calculateDeltaQ(const double &binWidth, std::string currentProbe)
{
    std::vector<long double> resultingCdf;
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

std::string ProbabilisticOperator::toString() const
{
    return "Probabilistic operator: " + name + "\n";
}

void ProbabilisticOperator::print(int depth, std::string currentProbe)
{
    std::cout << std::string(depth * 2, ' ') + "Probabilistic operator: " + getName() + "\n";
    int childIdx = 0;
    for (auto &child : children) {
        std::cout << std::string(depth * 2, ' ') + "Child: " << childIdx << "\n";
        child->print(depth + 1, currentProbe);
        childIdx++;
    }
    if (probeNextComponent.count(currentProbe))
        probeNextComponent.at(currentProbe)->print(depth, currentProbe);
}
