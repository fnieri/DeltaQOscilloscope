/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing a DeltaQ system
 */

#include "System.h"
#include "../Application.h"
#include <utility>

void System::setOutcomes(std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomesMap)
{
    outcomes = outcomesMap;
    for (auto &[name, outcome] : outcomes) {
        observables[name] = outcome;
    }
}

void System::setProbes(std::unordered_map<std::string, std::shared_ptr<Probe>> probesMap)
{
    probes = probesMap;
    for (auto &[name, probe] : probes) {
        observables[name] = probe;
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

void System::setObservableParameters(std::string &componentName, int exponent, int numBins)
{
    if (auto it = outcomes.find(componentName); it != outcomes.end()) {
        double maxDelay = it->second->setNewParameters(exponent, numBins);
        Application::getInstance().sendDelayChange(componentName, maxDelay * 1000);
    }
    if (auto it = probes.find(componentName); it != probes.end()) {
        double maxDelay = it->second->setNewParameters(exponent, numBins);
        Application::getInstance().sendDelayChange(componentName, maxDelay * 1000);
    }
}

void System::addSample(std::string &componentName, Sample &sample)
{
    if (auto it = observables.find(componentName); it != observables.end()) {
        it->second->addSample(sample);
    }
}

DeltaQ System::calculateDeltaQ()
{
    return DeltaQ();
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

[[nodiscard]] std::unordered_map<std::string, std::shared_ptr<Observable>> &System::getObservables()
{
    return observables;
}

bool System::hasOutcome(const std::string &name)
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

std::shared_ptr<Observable> System::getObservable(const std::string &name)
{
    return observables[name];
}

void System::setRecording(bool isRecording)
{
    if (recordingTrigger && isRecording) {
        return;
    }

    recordingTrigger = isRecording;
    for (auto &obs : observables) {
        if (obs.second) {
            obs.second->setRecording(isRecording);
        }
    }
}

bool System::isRecording() const
{
    return recordingTrigger;
}

void System::getObservablesSnapshotAt(std::uint64_t time)
{
    std::vector<Snapshot> result;
    for (auto &[name, observable] : observables) {
        if (observable)
            result.push_back(observable->getSnapshot());
    }

    snapshots[time] = std::move(result); // store the snapshots by timestamp
}

std::map<std::uint64_t, std::vector<Snapshot>> System::getAllSnapshots()
{
    return snapshots;
}
