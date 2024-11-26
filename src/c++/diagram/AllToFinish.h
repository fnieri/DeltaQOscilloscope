#include "DiagramComponent.h"
#include "System.h"

class AllToFinish : virtual public DiagramComponent {
private:
    std::vector<DiagramComponent> followingComponents;
    System system;

public:
    AllToFinish(std::string name, std::vector<DiagramComponent> followingComponents);

    /**
     * Assume two independent outcomes with the same start event
     * All-to-finish outcome occurs when both end events occur
     * All-to-finish is defined as ΔQ_{LTF(A,B)} ΔQ_A * ΔQ_B
    */
    DeltaQ calculateDeltaQ();
};