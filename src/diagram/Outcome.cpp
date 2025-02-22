#include "Outcome.h"
#include "DiagramComponent.h"
#include <iostream>
Outcome::Outcome(const std::string &name)
    : DiagramComponent(name)
    , Probe(name)
{
}

void Outcome::setNext(std::shared_ptr<DiagramComponent> next)
{
    nextComponent = next;
}

DeltaQ Outcome::calculateDeltaQ(const System &system, const DeltaQ &deltaQ)
{
    // return endEvent->calculateDeltaQ(system, getDeltaQ(system));
    return NULL;
}

DeltaQ Outcome::getDeltaQ(double binWidth) const
{
    std::vector<long double> outcomeSamples = getTimeSeries();
    return {binWidth, outcomeSamples};
}

std::string Outcome::toString() const
{
    return "Outcome: " + name + "\n";
}

void Outcome::print(int depth) const
{

    std::cout << std::string(depth * 2, ' ') << "Outcome: " << name << "\n";
    if (nextComponent)
        nextComponent->print(depth);
}
