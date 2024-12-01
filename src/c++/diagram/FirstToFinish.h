#pragma once

#include "DiagramComponent.h"
#include "../System.h"

class FirstToFinish final : virtual public DiagramComponent {
    std::vector<std::shared_ptr<DiagramComponent>> followingComponents;

public:
    FirstToFinish(const std::string& name, 
                const std::vector<std::shared_ptr<DiagramComponent>>& followingComponents);
    /**
     * Assume two independent outcomes with the same start event
     * First-to-finish outcome occurs when at least one end event occurs
     * We compute the probability that there are zero end events
     * First-to-finish is defined as    
     * ΔQ_{FTF(A,B)} = ΔQ_A + ΔQ_B – ΔQ_A * ΔQ_B
    */
    DeltaQ calculateDeltaQ(const System& system) override;
};