#include "ProbabilisticOperator.h"

ProbabilisticOperator::ProbabilisticOperator(std::string name, std::map<DiagramComponent, int> followingComponents) :
    DiagramComponent{name},
    followingComponentAndProbabilities{followingComponents}
    {}