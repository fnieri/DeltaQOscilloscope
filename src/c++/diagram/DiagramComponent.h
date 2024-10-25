/**
 * @file DiagramComponent.h
 * @author Francesco Nieri
 * @brief Abstract class representing an outcome diagram component
 * @date 25/10/2024 
 */

#include <string.h>

virtual class DiagramComponent { 

private:
    std::string name;

public:
    DiagramComponent(std::string name);

    /**
     * Return true only if the component's CDF can be plotted 
     */
    virtual bool isPlottable();

    virtual auto calculateDeltaQ(); // TODO change auto 

    std::string getName();
}