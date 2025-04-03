/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */

#include "System.h"
#include <execution>
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
    double max = 0;
    for (const auto &[name, outcome] : outcomes) {
        const double outcomeMax = outcome->getMax();
        if (outcomeMax > max) {
            max = outcomeMax;
        }
    }
    std::cout << max << " max \n";
    binWidth = max / N_OF_BINS;
}

void System::addSample(std::string &componentName, Sample &sample)
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
        std::cout << "added \n";
    }
}

double System::getBinWidth() const
{
    return binWidth;
}

DeltaQ System::calculateDeltaQ()
{
    calculateBinWidth();
    return firstComponent->calculateDeltaQ(binWidth, "system");
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
void System::toString() const
{
    if (firstComponent)
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

void System::setSystemDefinitionText(std::string &text)
{
    systemDefinitionText = text;
}

std::string System::getSystemDefinitionText()
{
    return systemDefinitionText;
}

void System::addSamplesBatch(const std::unordered_map<std::string, std::vector<Sample>> &batchSamples)
{
    std::for_each(std::execution::par_unseq, batchSamples.begin(), batchSamples.end(), [this](const auto &pair) { this->addSamples(pair.first, pair.second); });
}

void System::addSamples(const std::string &componentName, const std::vector<Sample> &samples)
{
    if (auto it = components.find(componentName); it != components.end()) {
        it->second->addSamples(samples); // Copy
    }
}
void System::addSamples(const std::string &componentName, std::vector<Sample> &&samples)
{
    if (auto it = components.find(componentName); it != components.end()) {
        it->second->addSamples(std::move(samples)); // Move
    }
}

void System::addSamplesBatch(std::unordered_map<std::string, std::vector<Sample>> &&batchSamples)
{
    std::for_each(std::execution::par_unseq, batchSamples.begin(), batchSamples.end(),
        [this](auto &&pair) { // Use forwarding reference
            this->addSamples(pair.first, std::move(pair.second));
        });
}

void System::replaceSystem(const System &other)
{
    if (this != &other) // Prevent self-assignment
    {

        outcomes.clear();
        operators.clear();
        probes.clear();
        components.clear();

        outcomes = other.outcomes;
        operators = other.operators;
        probes = other.probes;
        components = other.components;

        systemDefinitionText = other.systemDefinitionText;
        firstComponent = other.firstComponent;
        binWidth = other.binWidth;
    }
    for (auto &[name, component] : outcomes) {
        std::cout << name << "\n";
    }
}
