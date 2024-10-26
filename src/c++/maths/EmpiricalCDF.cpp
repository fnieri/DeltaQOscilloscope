/**
 * @author: Francesco Nieri
 * @date 26/10/2024
 * Class representing an Emprical
 */

#include <memory>
#include "Outcome.h"

struct EmpiricalCDF {
    std::unique_ptr<Outcome> outcome; 
    
    float min = 0;
    float max = 0;
    
    float mean = 0.0;
    float variance = 0.0;

    void computeStatistics();
};
        

void EmpiricalCDF::computeStatistics() {}