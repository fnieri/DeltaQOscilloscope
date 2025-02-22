#pragma once

#include "DiagramComponent.h"
#include "Operator.h"
#include "System.h"

class AllToFinish final : virtual public Operator
{

public:
    AllToFinish(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &nextComponents);

    AllToFinish(const std::string &name);
    /**
     * Assume two independent outcomes with the same start event
     * All-to-finish outcome occurs when both end events occur
     * All-to-finish is defined as ΔQ_{LTF(A,B)} ΔQ_A * ΔQ_B
     */
    DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) override;

    std::string toString() const;

    void print(int depth) const override;
};
