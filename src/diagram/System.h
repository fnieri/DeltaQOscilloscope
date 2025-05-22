/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */
#pragma once

#include "../maths/DeltaQ.h"
#include "Observable.h"
#include "Operator.h"
#include "Outcome.h"
#include "Probe.h"
#include <memory>
#include <unordered_map>

class System
{
    std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomes {};
    std::unordered_map<std::string, std::shared_ptr<Operator>> operators {};
    std::unordered_map<std::string, std::shared_ptr<Probe>> probes {};
    std::unordered_map<std::string, std::shared_ptr<Observable>> observables {};

    std::string systemDefinitionText;

    bool recordingTrigger = false;
    std::map<uint64_t, std::vector<Snapshot>> snapshots;

public:
    System() = default;

    [[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Outcome>> &getOutcomes();

    [[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Probe>> &getProbes();

    [[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Operator>> &getOperators();

    [[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Observable>> &getObservables();
    void setOutcomes(std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomesMap);

    void setOperators(std::unordered_map<std::string, std::shared_ptr<Operator>> operatorsMap);

    void setProbes(std::unordered_map<std::string, std::shared_ptr<Probe>> probesMap);

    bool hasOutcome(const std::string &name);

    bool hasOperator(const std::string &name);

    std::shared_ptr<Outcome> getOutcome(const std::string &outcomeName);

    std::shared_ptr<Operator> getOperator(const std::string &);

    bool hasProbe(const std::string &name);

    std::shared_ptr<Probe> getProbe(const std::string &name);

    std::shared_ptr<Observable> getObservable(const std::string &observableName);

    void setSystemDefinitionText(std::string &text);

    std::string getSystemDefinitionText();

    void setObservableParameters(std::string &, int, int);

    void addSample(std::string &componentName, Sample &sample);

    void setRecording(bool);

    bool isRecording() const;

    void getObservablesSnapshotAt(std::uint64_t);

    std::map<std::uint64_t, std::vector<Snapshot>> getAllSnapshots();

    std::vector<std::string> getAllComponentsName();

    /**
     * Calculate the resulting DeltaQ for the whole system
     */
    DeltaQ calculateDeltaQ();
};
