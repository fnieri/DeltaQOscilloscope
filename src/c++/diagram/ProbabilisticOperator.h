/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing a probabilstic operator in a DeltaQ system
 */

#include "Outcome.h"
#include "DeltaQ.h"

#include <map>

class ProbabilisticOperator : virtual public DiagramComponent {
private:
    std::map<DiagramComponent, double> followingComponentAndProbabilities;
    System system;
    
public:
    ProbabilisticOperator(std::string name, std::map<DiagramComponent, double> followingComponents);
    /**
     *  Assume there are two possible outcomes OA and OB and
     * exactly one outcome is chosen during each occurrence of a start event
     * O_A occurs with probability p/(p+q)
     * O_B occurs with probability q/(p+q)
     * Therefore:
     * ΔQ_{PC(A,B)} = p/(p+q) ΔQ_A + q/(p+q) ΔQ_B
    */
    DeltaQ calculateDeltaQ();

    bool isPlottable();
};