#include "DiagramComponent.h"
#include "System.h"

class FirstToFinish : virtual public DiagramComponent {
private:
    std::vector<DiagramComponent> followingComponents;
    System system;

public:
    FirstToFinish(std::string name, std::vector<DiagramComponent> followingComponents);
    
    /**
     * Assume two independent outcomes with the same start event
     * First-to-finish outcome occurs when at least one end event occurs
     * We compute the probability that there are zero end events
     * First-to-finish is defined as    
     * ΔQ_{FTF(A,B)} = ΔQ_A + ΔQ_B – ΔQ_A * ΔQ_B
    */
    DeltaQ calculateDeltaQ();
};