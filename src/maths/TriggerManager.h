#pragma once

#include "TriggerTypes.h"
#include "Triggers.h"
#include <memory>
#include <vector>

class TriggerManager {
public:
    struct Trigger {
        TriggerType type;
        TriggerDefs::Condition condition;
        TriggerDefs::Action action;
        bool enabled;

        Trigger(TriggerType t, TriggerDefs::Condition c,
                TriggerDefs::Action a, bool e = true);
    };

    void addTrigger(TriggerType type, TriggerDefs::Condition condition,
                   TriggerDefs::Action action, bool enabled = true);

    std::vector<Trigger> getTriggersByType(TriggerType type);
    void evaluate(const DeltaQ& dq, const QTA& qta) const;
    void removeTriggersByType(TriggerType type);
    void clearAllTriggers();
    void setTriggersEnabled(TriggerType type, bool enabled);

private:
    std::vector<Trigger> triggers_;
};