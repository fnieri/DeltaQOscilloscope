/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing a probabilstic operator in a DeltaQ system
 */

#include "DiagramComponent.h"
#include <map>

class ProbabilisticOperator : virtual public DiagramComponent {
private:
    std::map<DiagramComponent, int> followingComponentAndProbabilities;

public:
    ProbabilisticOperator(std::string name, std::map<DiagramComponent, int> followingComponents);

    void calculateDeltaQ();

    bool isPlottable();
};