#include "Operator.h"

#include <numeric>

#include "../maths/DeltaQOperations.h"
#include "Observable.h"
#include "OperatorType.h"
#include <cmath>
#include <limits>
#define OP_EPSILON std::numeric_limits<double>::epsilon()

Operator::Operator(const std::string &name, OperatorType type)
    : Observable(name)
    , type(type)
    , calculatedInterval(50)
{
}

Operator::~Operator() = default;

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

DeltaQ Operator::calculateCalculatedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    if (observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound).has_value()) {
        return observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound).value().deltaQ;
    }
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
    DeltaQ result;

    // Choose appropriate operation
    if (type == OperatorType::FTF)
        result = firstToFinish(deltaQs);
    else if (type == OperatorType::PRB)
        result = probabilisticChoice(probabilities, deltaQs);
    else
        result = allToFinish(deltaQs);

    int calculatedBins = result.getBins();

    if (calculatedBins != calculatedInterval.getBins()) {
        calculatedInterval.setNumBins(calculatedBins);
        calculatedDeltaQHistory.clear();
    }

    calculatedInterval.addDeltaQ(result);
    calculatedDeltaQHistory.push_back(result);

    if (calculatedDeltaQHistory.size() > MAX_DQ) {
        calculatedInterval.removeDeltaQ(calculatedDeltaQHistory.front());
        calculatedDeltaQHistory.pop_front();
    }

    observableSnapshot.addCalculatedDeltaQ(timeLowerBound, result, calculatedInterval.getBounds());

    if (!recording && calculatedDeltaQHistory.size() > MAX_DQ) {
        observableSnapshot.removeOldestCalculatedDeltaQ();
    }
    return result;
}

DeltaQRepr Operator::getCalculatedDeltaQRepr(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::lock_guard<std::mutex> lock(calcMutex);
    auto deltaQRepr = observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound);
    if (!deltaQRepr.has_value()) {
        calculateCalculatedDeltaQ(timeLowerBound, timeUpperBound);
        deltaQRepr = observableSnapshot.getCalculatedDeltaQAtTime(timeLowerBound);
    }
    return deltaQRepr.value();
}
