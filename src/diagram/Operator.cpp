#include "Operator.h"

#include <numeric>

#include "../maths/DeltaQOperations.h"
#include "DiagramComponent.h"
#include "OperatorType.h"
#include <cmath>
#include <limits>
#define OP_EPSILON std::numeric_limits<double>::epsilon()

Operator::Operator(const std::string name, OperatorType type)
    : DiagramComponent(name)
    , type(type)
{
}

void Operator::setProbabilities(const std::vector<double> &probs)
{
    if (type != OperatorType::PRB) {
        throw std::invalid_argument("Only probabilistic operators accept probabilities");
    }

    double result = std::reduce(probs.begin(), probs.end());

    if (std::fabs(1 - result) > OP_EPSILON)
        throw std::logic_error("Result should approximate to 1");

    probabilities = probs;
}

DeltaQ Operator::calculateObservedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::vector<DeltaQ> deltaQs;

    // Get DeltaQ for all children
    for (auto &childrenLinks : causalLinks) {
        std::vector<DeltaQ> childrenDeltaQs;

        // For each children in causal link, get their DeltaQ
        for (auto &component : childrenLinks) {
            childrenDeltaQs.push_back(component->getObservedDeltaQ(timeLowerBound, timeUpperBound));
        }
        // Get the convolution of the components in a children
        deltaQs.push_back(convolveN(childrenDeltaQs));
    }

    DeltaQ observableDeltaQ;

    // Choose appropriate operation
    if (type == OperatorType::FTF)
        observableDeltaQ = firstToFinish(deltaQs);
    else if (type == OperatorType::PRB)
        observableDeltaQ = probabilisticChoice(probabilities, deltaQs);
    else
        observableDeltaQ = allToFinish(deltaQs);

    observedDeltaQs[timeLowerBound] = observableDeltaQ;

    if (observedDeltaQs.size() > 10) {
        auto earliest = observedDeltaQs.begin();
        observedDeltaQs.erase(earliest);
    }

    return observableDeltaQ;
}

DeltaQ Operator::getObservedDeltaQ(std::uint64_t timeLowerBound, std::uint64_t timeUpperBound)
{
    if (observedDeltaQs.count(timeLowerBound)) {
        return observedDeltaQs[timeLowerBound];
    }
    return calculateObservedDeltaQ(timeLowerBound, timeUpperBound);
}
