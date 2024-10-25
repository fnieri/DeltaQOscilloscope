/**
* @author Francesco Nieri
* 
*
 */

#include "DiagramComponent.h"

class ProbabilisticOperator : virtual public DiagramComponent {
private:
    std::map<DiagramComponent, int> followingComponentAndProbabilities;

public:
    ProbabilisticOperator(std::string name, std::map<DiagramComponent, int> followingComponents);

    auto calculateDeltaQ();

    bool isPlottable();
}