#include "Operator.h"

#include <numeric>

#include "DiagramComponent.h"
#include "OperatorType.h"
#include "../maths/DeltaQOperations.h"
#include <limits>
#include <cmath>
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

DeltaQ Operator::calculateObservableDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound) {
    std::vector<DeltaQ> deltaQs;
    for (auto &childrenLinks : causalLinks) {
        std::vector<DeltaQ> childrenDeltaQs;
        for (auto &component : childrenLinks) {
            childrenDeltaQs.push_back(component->getObservableDeltaQ(timeLowerBound, timeUpperBound));
        }
        deltaQs.push_back(convolveN(childrenDeltaQs));
    }
    DeltaQ observableDeltaQ;

    if (type == OperatorType::FTF)
        observableDeltaQ = firstToFinish(deltaQs);
    else if (type == OperatorType::PRB)
        observableDeltaQ = probabilisticChoice(probabilities, deltaQs);
    else
        observableDeltaQ = allToFinish(deltaQs);
    observedDeltaQs[timeLowerBound] = observableDeltaQ;
    if (observedDeltaQs.size() > 30) {
        auto earliest = observedDeltaQs.begin();
        observedDeltaQs.erase(earliest);
    }
    return observableDeltaQ;
}
