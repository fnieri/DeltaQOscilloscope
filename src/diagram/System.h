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
    std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomes {}; ///< All outcome
    std::unordered_map<std::string, std::shared_ptr<Operator>> operators {}; ///< All operators
    std::unordered_map<std::string, std::shared_ptr<Probe>> probes {}; /// < All probes
    std::unordered_map<std::string, std::shared_ptr<Observable>> observables {}; ///< The above grouped together

    std::string systemDefinitionText; ///< The definition of the system

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

    /**
     * @brief Add outcome instance for an observable, if it exists
     */
    void addSample(std::string &componentName, Sample &sample);

    /**
     * @brief Set all observables to record snapshots
     */
     void setRecording(bool);

    bool isRecording() const;

    /**
     * @brief Add the snapshots of all observables for a trigger at time t
     */
    void getObservablesSnapshotAt(std::uint64_t);

    /**
     * Get the snapshots of all observables for a trigger at time t
     */
    std::map<std::uint64_t, std::vector<Snapshot>> getAllSnapshots();

    /**
     * @brief Get the name of all components
     */
    std::vector<std::string> getAllComponentsName();

    /**
     * @deprecated This may be used in the future
     * Calculate the resulting DeltaQ for the whole system
     */
    DeltaQ calculateDeltaQ();
};
