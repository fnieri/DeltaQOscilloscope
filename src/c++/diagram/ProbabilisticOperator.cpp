/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing a probabilstic operator in a DeltaQ system
 */


#include "ProbabilisticOperator.h"

ProbabilisticOperator::ProbabilisticOperator(std::string name, std::map<DiagramComponent, int> followingComponents) :
    DiagramComponent{name},
    followingComponentAndProbabilities{followingComponents}
    {}