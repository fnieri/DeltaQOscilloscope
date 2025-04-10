#pragma once

#include "DiagramComponent.h"
#include "Operator.h"
#include "System.h"
#include <vector>

class FirstToFinish final : public Operator
{

public:
    FirstToFinish(const std::string &name);

    FirstToFinish(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &children);

    /**
     * Assume two independent outcomes with the same start event
     * First-to-finish outcome occurs when at least one end event occurs
     * We compute the probability that there are zero end events
     * First-to-finish is defined as
     * ΔQ_{FTF(A,B)} = ΔQ_A + ΔQ_B – ΔQ_A * ΔQ_B
     */
    [[nodiscard]] DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound) override;
};
