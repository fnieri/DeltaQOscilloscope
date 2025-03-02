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

public:
    ProbabilisticOperator(const std::string &name);

    ProbabilisticOperator(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &children);
    /**
     * Assume there are two possible outcomes OA and OB and
     * exactly one outcome is chosen during each occurrence of a start event
     * O_A occurs with probability p/(p+q)
     * O_B occurs with probability q/(p+q)
     * Therefore:
     * ΔQ_{PC(A,B)} = p/(p+q) ΔQ_A + q/(p+q) ΔQ_B
     */
    DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) override;

    std::string toString() const override;

    void print(int depth, std::string currentProbe) override;
};
