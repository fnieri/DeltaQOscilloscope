/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */
#pragma once

#include "../maths/DeltaQ.h"

#include "DiagramComponent.h"
#include "Observable.h"
#include "Operator.h"
#include "Outcome.h"
#include "Probe.h"
#include <memory>
#include <unordered_map>

class System
{
    std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomes;
    std::unordered_map<std::string, std::shared_ptr<Operator>> operators;
    std::unordered_map<std::string, std::shared_ptr<Probe>> probes;
    std::unordered_map<std::string, std::shared_ptr<Observable>> components;

    std::string systemDefinitionText;
    std::shared_ptr<DiagramComponent> firstComponent;

public:
    void calculateAllComponentsDeltaQ(uint64_t, uint64_t);

    System() = default;

    void setFirstComponent(std::shared_ptr<DiagramComponent> component);

    [[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Outcome>> &getOutcomes();

    [[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Probe>> &getProbes();

    [[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Operator>> &getOperators();

    void setOutcomes(std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomesMap);

    void setOperators(std::unordered_map<std::string, std::shared_ptr<Operator>> operatorsMap);

    void setProbes(std::unordered_map<std::string, std::shared_ptr<Probe>> probesMap);

    void addSample(std::string &componentName, Sample &sample);

    std::shared_ptr<Outcome> getOutcome(const std::string &outcomeName);

    std::vector<std::string> getAllComponentsName();
    /**
     * Calculate the resulting DeltaQ for the whole system
     */
    DeltaQ calculateDeltaQ();

    bool hasOutcome(std::string &name);

    bool hasProbe(const std::string &name);

    std::shared_ptr<Probe> getProbe(const std::string &name);

    void setSystemDefinitionText(std::string &text);

    std::string getSystemDefinitionText();

    void setObservableParameters(std::string &, int, int);
};
