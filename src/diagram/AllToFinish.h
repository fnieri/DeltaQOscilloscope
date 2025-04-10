#pragma once

#include "DiagramComponent.h"
#include "Operator.h"
#include "System.h"

class AllToFinish final : public Operator
{

public:
    AllToFinish(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &children);

    AllToFinish(const std::string &name);
    /**
     * Assume two independent outcomes with the same start event
     * All-to-finish outcome occurs when both end events occur
     * All-to-finish is defined as ΔQ_{LTF(A,B)} ΔQ_A * ΔQ_B
     */
    [[nodiscard]] DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe, uint64_t timeLowerBound, uint64_t timeUpperBound) override;
};
