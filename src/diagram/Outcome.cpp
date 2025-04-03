#include "Outcome.h"
#include "../maths/DeltaQOperations.h"
#include "DiagramComponent.h"
#include "System.h"
#include <iostream>

Outcome::Outcome(const std::string &name)
    : DiagramComponent(name)
    , Observable(name)
{
}

DeltaQ Outcome::calculateDeltaQ(const double &binWidth, std::string currentProbe)
{
    if (probeNextComponent.count(currentProbe)) {

        return convolve(getDeltaQ(binWidth), probeNextComponent.at(currentProbe)->calculateDeltaQ(binWidth, currentProbe));
    }
    return getDeltaQ(binWidth);
}
/*
DeltaQ Outcome::getDeltaQ(double binWidth) const
{
    std::vector<long double> outcomeSamples = getTimeSeries();
    return {binWidth, outcomeSamples};
}
*/

DeltaQ Outcome::getDeltaQ(double binWidth) const
{
    return {binWidth, getSamplesInRange(0, 10000000000000000000)}; // FIXME
}
std::string Outcome::toString() const
{
    return "Outcome: " + name + "\n";
}

void Outcome::print(int depth, std::string currentProbe)
{
    std::cout << std::string(depth * 2, ' ') << "Outcome: " << name << "\n";

    auto it = probeNextComponent.find(currentProbe);
    if (it != probeNextComponent.end()) {
        it->second->print(depth, currentProbe);
    }
}
