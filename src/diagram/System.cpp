/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */

#include "System.h"
#include <utility>

void System::setFirstComponent(std::shared_ptr<DiagramComponent> component)
{
    firstComponent = std::move(component);
}

void System::setOutcomes(std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomesMap)
{
    outcomes = outcomesMap;
    for (auto &[name, outcome] : outcomes) {
        components[name] = outcome;
    }
}

void System::setProbes(std::unordered_map<std::string, std::shared_ptr<Probe>> probesMap)
{
    probes = probesMap;
    for (auto &[name, probe] : probes) {
        components[name] = probe;
    }
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
    binWidth = 0.001; // FIXME
}

void System::addSample(std::string &componentName, Sample &sample)
{
    if (auto it = outcomes.find(componentName); it != outcomes.end()) {
        it->second->addSample(sample);
    }
    if (auto it = probes.find(componentName); it != probes.end()) {
        it->second->addSample(sample);
    }
}

double System::getBinWidth() const
{
    return binWidth;
}

DeltaQ System::calculateDeltaQ()
{
    calculateBinWidth();
    return firstComponent->calculateDeltaQ(binWidth, "system", 0, 0);
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

bool System::hasOutcome(std::string &name)
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

std::vector<std::string> System::getAllComponentsName()
{
    std::vector<std::string> names;
    int probesSize = probes.size();
    int outcomesSize = outcomes.size();

    names.reserve(probesSize + outcomesSize);
    for (auto &[name, _] : getProbes())
        names.push_back(name);
    for (auto &[name, k] : getOutcomes())
        names.push_back(name);
    return names;
}

void System::setSystemDefinitionText(std::string &text)
{
    systemDefinitionText = text;
}

std::string System::getSystemDefinitionText()
{
    return systemDefinitionText;
}
