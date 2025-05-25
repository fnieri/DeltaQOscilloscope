#pragma once

#include "../maths/DeltaQ.h"
#include "Observable.h"
#include "OperatorType.h"

/**
 * @class Operator This class represents an operator according to the DeltaQSD paradigm
 */
class Operator : public Observable
{
    OperatorType type;

    std::vector<double> probabilities; ///< The probabilities of the components inside the operator, only available for probabilistic operator

    std::vector<std::vector<std::shared_ptr<Observable>>> causalLinks; ///< The causal links for each children

    ConfidenceInterval calculatedInterval;

    std::mutex calcMutex;

    std::deque<DeltaQ> calculatedDeltaQHistory; ///< History for confidence intervals

public:
    Operator(const std::string &name, OperatorType);

    ~Operator();
    /**
     * @brief calculate a Calculated deltaQ with bounds timeLowerBound, timeUpperBound
     * @param timeLowerBound
     * @param timeUpperBound
     * @return calculated DeltaQ
     */
    DeltaQ calculateCalculatedDeltaQ(uint64_t, uint64_t);

    /**
     * @brief Get the representation of a calculated DeltaQ for plotting
     * @return the representation of a calculated DeltaQ
     */
    DeltaQRepr getCalculatedDeltaQRepr(uint64_t, uint64_t);

    void setProbabilities(const std::vector<double> &);

    /**
     * @brief Get the probabilities of a probabilistic operator
     * @return The probabilities
    */
    std::vector<double> getProbabilities()
    {
        return probabilities;
    }

    /**
     * @brief Get the links for a children
     * @return The links for a children
     */
    std::vector<std::shared_ptr<Observable>> getChildren();

    void setCausalLinks(std::vector<std::vector<std::shared_ptr<Observable>>> links)
    {
        causalLinks = links;
    }

    std::vector<std::vector<std::shared_ptr<Observable>>> getCausalLinks()
    {
        return causalLinks;
    }

    OperatorType getType()
    {
        return type;
    }
};
