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
    DeltaQ calculateDeltaQ(const double &binWidth, std::string currentProbe) override;

    std::string toString() const;

    void print(int depth, std::string currentProbe) override;
};
