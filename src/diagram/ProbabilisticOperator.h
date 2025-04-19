/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing a probabilstic operator in a DeltaQ system
 */
#pragma once

#include "DiagramComponent.h"

#include "../maths/DeltaQ.h"
#include "Operator.h"
#include "System.h"

class ProbabilisticOperator : public Operator
{
    std::vector<double> probabilities;

public:
    ProbabilisticOperator(const std::string &name);

    ProbabilisticOperator(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &children);

    void setProbabilities(std::vector<double> &);
    /**
     * Assume there are two possible outcomes OA and OB and
     * exactly one outcome is chosen during each occurrence of a start event
     * O_A occurs with probability p/(p+q)
     * O_B occurs with probability q/(p+q)
     * Therefore:
     * ΔQ_{PC(A,B)} = p/(p+q) ΔQ_A + q/(p+q) ΔQ_B
     */

    [[nodiscard]] DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound) override;
};
