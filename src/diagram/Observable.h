#pragma once

#include "../maths/Snapshot.h"
#include "../maths/TriggerManager.h"
#include "Sample.h"
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
    void updateSnapshot(uint64_t timeLowerBound, DeltaQ &deltaQ);

public:
    /**
     * @brief Constructor an observable with its name
     */
    Observable(const std::string &name);

    virtual ~Observable() { };
    /**
     * Add a sample (outcome instance) to an observable
     */
    void addSample(const Sample &sample);
    /**
     * @brief Get all sample with endTime timeLowerBound - timeUpperBound
     * @return The sample in range timeLowerBound - timeUpperBound
     */
    std::vector<Sample> getSamplesInRange(std::uint64_t timeLowerBound, std::uint64_t timeUpperBound);
    /**
     * @brief Get observed DeltaQ in range timeLowerBound - timeUpperBound from snapshot, if it has not been calculated, calculate it
     * @return DeltaQ
     */
    DeltaQ getObservedDeltaQ(uint64_t, uint64_t);
    /**
     * @brief Calculate the observed DeltaQ in range timeLowerBound - timeUpperBound, add it to snapshot and ConfidenceInterval
     * @return The calculated DeltaQ
     */
    DeltaQ calculateObservedDeltaQ(uint64_t, uint64_t);

    /**
     * @brief Get DeltaQ representation for graphical plotting
     */
    DeltaQRepr getObservedDeltaQRepr(uint64_t, uint64_t);

    /**
     * @brief Set new parameters for a DeltaQ
     * @return new dMax
     */
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

    /**
     * @brief Set recoding snapshot
     * @param bool is recording
     */
    void setRecording(bool);

    /**
     * @brief Set QTA for an observable
     * @param qta new QTA
     */
    void setQTA(const QTA &);
    /**
     * @brief add a trigger to an observable
     * @param type the observable type
     * @param condition the condition to evalute
     * @param action action to perform on trigger fired
     * @param enabled
     * @param sampleLimit sampleLimit sample limit for sample limit trigger
     */
    void addTrigger(TriggerType type, TriggerDefs::Condition condition, TriggerDefs::Action action, bool enabled, std::optional<int> sampleLimit);
    /**
     * @brief Remove observable trigger
     */
    void removeTrigger(TriggerType type);

    /**
     * @brief Get snapshot of observable
     */
    Snapshot getSnapshot();
    /**
     * @brief Get observable name
     * @return its name
     */
    [[nodiscard]] std::string getName() const &;
};
