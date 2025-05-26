#pragma once

#include "TriggerTypes.h"
#include "Triggers.h"
#include <memory>
#include <optional>
#include <vector>

/**
 * @class TriggerManager
 * @brief Manages triggers for an observable
 */
class TriggerManager
{
public:
    struct Trigger {
        TriggerType type;

        TriggerDefs::Condition condition;

        TriggerDefs::Action action;

        bool enabled;

        std::optional<int> sampleLimitValue;

        Trigger(TriggerType t, TriggerDefs::Condition c, TriggerDefs::Action a, bool e = true);
    };
    /**
     * @brief Add a trigger for an observable,
     * @param type The type of the trigger
     * @param condition The condition for the trigger to be fired
     * @param action The action to perform when fired
     * @param enabled It the trigger is enabled
     */
    void addTrigger(TriggerType type, TriggerDefs::Condition condition, TriggerDefs::Action action, bool enabled = true);

    /**
     *  @brief Get all triggers set for a type
     */
    std::vector<Trigger> getTriggersByType(TriggerType type);

    /**
     * @brief Evaluate a DeltaQ to see if a trigger should be fired
     * @param dq The DeltaQ to evaluate
     * @param qta The qta to compare against to
     * @param std::uint64_t Keep the time to log it if the trigger fires
     */
    void evaluate(const DeltaQ &dq, const QTA &qta, std::uint64_t) const;

    /**
     * @brief Remove triggers if their type matches the param type
     * @param type The trigger's type'
     */
    void removeTriggersByType(TriggerType type);

    /**
     * @brief Remove all triggers
     */
    void clearAllTriggers();

    /**
     * @brief Enable/Disable all triggers with a type
     * @param type The type of trigger to enable/disable
     * @param enabled
     */
    void setTriggersEnabled(TriggerType type, bool enabled);

    /**
     * @brief Get all triggers
     */
    std::vector<Trigger> getAllTriggers() const;

private:
    std::vector<Trigger> triggers_;
};
