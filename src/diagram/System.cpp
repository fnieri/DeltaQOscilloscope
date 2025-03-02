/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */

#include "System.h"

#include "Outcome.h"
#include <iostream>
#include <utility>
#define N_OF_BINS 10.0

void System::setFirstComponent(std::shared_ptr<DiagramComponent> component)
{
    firstComponent = std::move(component);
}

void System::setOutcomes(std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomesMap)
{
    outcomes = outcomesMap;
}

void System::setOperators(std::unordered_map<std::string, std::shared_ptr<Operator>> operatorsMap)
{
    operators = operatorsMap;
}
std::shared_ptr<Outcome> System::getOutcome(const std::string &name)
{
    return outcomes[name];
}

void System::calculateBinWidth()
{
    double max = 0;
    for (const auto &[name, outcome] : outcomes) {
        const double outcomeMax = outcome->getMax();
        if (outcomeMax > max) {
            max = outcomeMax;
        }
    }
    binWidth = max / N_OF_BINS;
}

void System::addSample(std::string &componentName, std::pair<long long, long long> sample)
{
    auto findOutcome = outcomes.find(componentName);
    if (findOutcome != outcomes.end()) {
        auto component = outcomes[componentName];
        component->addSample(sample);
    }
    auto findProbe = probes.find(componentName);
    if (findProbe != probes.end()) {
        auto component = probes[componentName];
        component->addSample(sample);
    }
}

double System::getBinWidth() const
{
    return binWidth;
}

DeltaQ System::calculateDeltaQ()
{
    calculateBinWidth();
    return firstComponent->calculateDeltaQ(*this, DeltaQ());
}

[[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Outcome>> &System::getOutcomes()
{
    return outcomes;
}

[[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Operator>> &System::getOperators()
{
    return operators;
}

[[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Probe>> &System::getProbes()
{
    return probes;
}

bool System::containsOutcome(std::string &name)
{
    return outcomes.find(name) != outcomes.end();
}

bool System::hasProbe(const std::string &name)
{
    return probes.find(name) != probes.end();
}

std::shared_ptr<Probe> System::getProbe(const std::string &name)
{
    return probes[name];
}

void System::setProbes(std::unordered_map<std::string, std::shared_ptr<Probe>> probesMap)
{
    probes = probesMap;
}

void System::toString() const
{
    std::cout << "Printing system" << "\n";
    firstComponent->print(0, "system");
}

void System::toString(const std::string &probeName) const
{
    probes.at(probeName)->print(0, probeName);
}

std::vector<std::string> System::getAllComponentsName()
{
    std::vector<std::string> names;
    int probesSize = probes.size();
    int outcomesSize = outcomes.size();
    // TODO add operators
    names.reserve(probesSize + outcomesSize);
    for (auto &[name, _] : getProbes())
        names.push_back(name);
    for (auto &[name, k] : getOutcomes())
        names.push_back(name);
    return names;
}
