/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing a probabilstic operator in a DeltaQ system
 */

#include "ProbabilisticOperator.h"
#include "../maths/DeltaQOperations.h"
#include <cassert>
#include <iostream>
#include <limits>
#include <math.h>
#include <numeric>
#include <stdexcept>
#define EPSILON std::numeric_limits<double>::epsilon()

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

DeltaQ ProbabilisticOperator::calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    return DeltaQ();
}

void ProbabilisticOperator::setProbabilities(std::vector<double> &newProbabilities)
{
    double result = std::reduce(newProbabilities.begin(), newProbabilities.end());
    if (std::fabs(1 - result) > EPSILON)
        throw std::logic_error("Result should approximate to 1");
    probabilities = newProbabilities;
}

/*
DeltaQ ProbabilisticOperator::calculateDeltaQ(const double &binWidth, std::string currentProbe)
{
    std::vector<double> resultingCdf;
    std::vector<DeltaQ> scaledDeltaQs;

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

    // TODO Calculate Probabilities
    return {0, resultingCdf, false};
}
*/
