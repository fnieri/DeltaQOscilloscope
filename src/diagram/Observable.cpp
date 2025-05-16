#include "Observable.h"
#include "DiagramComponent.h"
#include <algorithm>
#include <cmath>
#include <iostream>
Observable::Observable(const std::string &name)
    : DiagramComponent(name)
    , observedInterval(50)
{
}
void Observable::addSample(const Sample &sample)
{
    samples.emplace_back(sample);
    sorted = false;
}

std::vector<Sample> Observable::getSamplesInRange(std::uint64_t lowerTime, std::uint64_t upperTime)
{
    std::lock_guard<std::mutex> lock(samplesMutex);
    if (!sorted) {
        std::sort(samples.begin(), samples.end(), [](const Sample &a, const Sample &b) { return a.startTime < b.startTime; });
        sorted = true;
    }

    std::vector<Sample> selectedSamples;

    auto lower = std::lower_bound(samples.begin(), samples.end(), lowerTime, [](const Sample &s, long long time) { return s.startTime < time; });

    auto upper = std::upper_bound(samples.begin(), samples.end(), upperTime, [](long long time, const Sample &s) { return time < s.startTime; });

    for (auto it = lower; it != upper; ++it) {
        selectedSamples.emplace_back(*it);
    }
    samples.erase(std::remove_if(samples.begin(), samples.end(), [upperTime](const Sample &s) { return s.startTime < upperTime; }), samples.end());

    return selectedSamples;
}

void Observable::setQTA(const QTA &newQTA)
{
    if (newQTA.perc_25 > maxDelay || newQTA.perc_50 > maxDelay || newQTA.perc_75 > maxDelay)
        throw std::invalid_argument("Percentages should not be bigger than maximum delay " + std::to_string(maxDelay));
    qta = newQTA;
}

void Observable::addTrigger(TriggerType type, TriggerDefs::Condition condition, TriggerDefs::Action action, bool enabled, std::optional<int> sampleLimitVal)
{
    auto &tm = triggerManager;
    tm.addTrigger(type, condition, action, enabled);
    auto &trigger = tm.getTriggersByType(type).back();
    trigger.sampleLimitValue = sampleLimitVal;
}

void Observable::removeTrigger(TriggerType type)
{
    triggerManager.removeTriggersByType(type);
}

void Observable::setRecording(bool isRecording)
{
    recording = isRecording;
    if (!isRecording) {
        observableSnapshot.resizeTo(30); // FIXME magic numbah
    }
}
