#include "Observable.h"
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <iostream>

#define MAX_DQ 30
Observable::Observable(const std::string &name)
    : observedInterval(50)
    , name(name)
{
    observableSnapshot.setName(name);
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
        std::sort(samples.begin(), samples.end(), [](const Sample &a, const Sample &b) { return a.endTime < b.endTime; });
        sorted = true;
    }

    std::vector<Sample> selectedSamples;

    auto lower = std::lower_bound(samples.begin(), samples.end(), lowerTime, [](const Sample &s, long long time) { return s.endTime < time; });

    auto upper = std::upper_bound(samples.begin(), samples.end(), upperTime, [](long long time, const Sample &s) { return time < s.endTime; });

    for (auto it = lower; it != upper; ++it) {
        selectedSamples.emplace_back(*it);
    }
    samples.erase(std::remove_if(samples.begin(), samples.end(), [upperTime](const Sample &s) { return s.endTime < upperTime; }), samples.end());

    return selectedSamples;
}

DeltaQ Observable::calculateObservedDeltaQ(std::uint64_t timeLowerBound, std::uint64_t timeUpperBound)
{
    auto samplesInRange = getSamplesInRange(timeLowerBound, timeUpperBound);
    if (samplesInRange.empty()) {
        DeltaQ deltaQ = DeltaQ();
        updateSnapshot(timeLowerBound, deltaQ);
        return deltaQ;
    }

    DeltaQ deltaQ {getBinWidth(), samplesInRange, nBins};
    updateSnapshot(timeLowerBound, deltaQ);

    triggerManager.evaluate(deltaQ, qta, timeLowerBound);
    return deltaQ;
}

void Observable::updateSnapshot(uint64_t timeLowerBound, DeltaQ &deltaQ)
{
    if (!(deltaQ == DeltaQ())) {
        observedInterval.addDeltaQ(deltaQ);
        confidenceIntervalHistory.push_back(deltaQ);

        if (confidenceIntervalHistory.size() > MAX_DQ) {
            observedInterval.removeDeltaQ(confidenceIntervalHistory.front());
            confidenceIntervalHistory.pop_front();
        }
    }
    observableSnapshot.addObservedDeltaQ(timeLowerBound, deltaQ, observedInterval.getBounds());
    observableSnapshot.addQTA(timeLowerBound, qta);
    if (!recording && (observableSnapshot.getObservedSize() > MAX_DQ)) {
        observableSnapshot.resizeTo(MAX_DQ);
    }
}

DeltaQ Observable::getObservedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::lock_guard<std::mutex> lock(observedMutex);
    auto deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    if (!deltaQRepr.has_value()) {
        calculateObservedDeltaQ(timeLowerBound, timeUpperBound);
        deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    }
    return deltaQRepr.value().deltaQ;
}

DeltaQRepr Observable::getObservedDeltaQRepr(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::lock_guard<std::mutex> lock(observedMutex);
    auto deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    if (!deltaQRepr.has_value()) {
        calculateObservedDeltaQ(timeLowerBound, timeUpperBound);
        deltaQRepr = observableSnapshot.getObservedDeltaQAtTime(timeLowerBound);
    }
    return deltaQRepr.value();
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
    if (recording && !isRecording) {
        recording = isRecording;
        observableSnapshot.resizeTo(30); // FIXME magic numb
    }
    recording = isRecording;
}

Snapshot Observable::getSnapshot()
{
    return observableSnapshot;
}

double Observable::setNewParameters(int newExp, int newNBins)
{
    std::lock_guard<decltype(paramMutex)> lock(paramMutex);

    bool binsChanged = (newNBins != nBins);
    bool expChanged = (newExp != deltaTExp);

    nBins = newNBins;
    deltaTExp = newExp;

    maxDelay = DELTA_T_BASE * std::pow(2, deltaTExp) * nBins;

    if (qta.perc_25 > maxDelay || qta.perc_50 > maxDelay || qta.perc_75 > maxDelay) {
        qta = QTA::create(0, 0, 0, qta.cdfMax, false);
    }

    if (binsChanged || expChanged) {
        observedInterval.setNumBins(nBins);
        confidenceIntervalHistory.clear();
    }

    return maxDelay;
}

std::string Observable::getName() const &
{
    return name;
}
