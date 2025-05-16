#pragma once

#include "TriggerTypes.h"
#include "Triggers.h"
#include <memory>
#include <optional>
#include <vector>

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

    void addTrigger(TriggerType type, TriggerDefs::Condition condition, TriggerDefs::Action action, bool enabled = true);

    std::vector<Trigger> getTriggersByType(TriggerType type);

    void evaluate(const DeltaQ &dq, const QTA &qta, std::uint64_t) const;

    void removeTriggersByType(TriggerType type);

    void clearAllTriggers();

    void setTriggersEnabled(TriggerType type, bool enabled);

    std::vector<Trigger> getAllTriggers() const;

private:
    std::vector<Trigger> triggers_;
};
