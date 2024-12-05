#pragma once

#include "DiagramComponent.h"
#include "System.h"

class AllToFinish final : virtual public DiagramComponent
{
    std::vector<std::shared_ptr<DiagramComponent>> followingComponents;

public:
    AllToFinish(const std::string &name, const std::vector<std::shared_ptr<DiagramComponent>> &followingComponents);
    /**
     * Assume two independent outcomes with the same start event
     * All-to-finish outcome occurs when both end events occur
     * All-to-finish is defined as ΔQ_{LTF(A,B)} ΔQ_A * ΔQ_B
     */
    DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) override;

    bool isPlottable() override
    {
        return false;
    }
};