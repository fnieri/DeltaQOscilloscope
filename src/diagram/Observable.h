#pragma once

#include "../maths/Snapshot.h"
#include "Sample.h"
#include "src/maths/TriggerManager.h"
#include <deque>
#include <math.h>
#include <mutex>

#define DELTA_T_BASE 0.001
#define MAX_DQ 30
class Observable
{
protected:
    std::string name;
    std::deque<Sample> samples;
    mutable bool sorted;

    std::deque<DeltaQ> confidenceIntervalHistory;
    double maxDelay {0.05};
    int deltaTExp {0}; // Exponent for dynamic binning
    int nBins {50}; // Number of bins

    TriggerManager triggerManager;

    ConfidenceInterval observedInterval;
    QTA qta;

    Snapshot observableSnapshot;

    std::mutex observedMutex;
    std::mutex samplesMutex;
    std::mutex paramMutex;

    bool recording = false;

private:
    DeltaQ calculateObservedDeltaQ(uint64_t, uint64_t);

public:
    Observable(const std::string &name);

    virtual ~Observable() { };
    void addSample(const Sample &sample);

    std::vector<Sample> getSamplesInRange(std::uint64_t timeLowerBound, std::uint64_t timeUpperBound);

    DeltaQ getObservedDeltaQ(uint64_t, uint64_t);

    DeltaQRepr getObservedDeltaQRepr(uint64_t, uint64_t);

    double setNewParameters(int newExp, int newNBins);

    double getBinWidth() const
    {
        return DELTA_T_BASE * std::pow(2, deltaTExp);
    }

    int getNBins() const
    {
        return nBins;
    }

    double getMaxDelay() const
    {
        return maxDelay;
    }

    QTA getQTA() const
    {
        return qta;
    }

    int getDeltaTExp() const
    {
        return deltaTExp;
    }

    const TriggerManager &getTriggerManager() const
    {
        return triggerManager;
    }

    void setRecording(bool);

    void setQTA(const QTA &newQTA);

    void addTrigger(TriggerType type, TriggerDefs::Condition condition, TriggerDefs::Action action, bool enabled, std::optional<int> sampleLimit);

    void removeTrigger(TriggerType type);

    Snapshot getSnapshot();

    [[nodiscard]] std::string getName() const &;
};
