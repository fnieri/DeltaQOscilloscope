#include "TriggerManager.h"

#include <algorithm>

TriggerManager::Trigger::Trigger(TriggerType t, TriggerDefs::Condition c,
                                 TriggerDefs::Action a, bool e)
    : type(t), condition(std::move(c)), action(std::move(a)), enabled(e) {}

void TriggerManager::addTrigger(TriggerType type, TriggerDefs::Condition condition,
                              TriggerDefs::Action action, bool enabled) {
    triggers_.emplace_back(type, std::move(condition),
                                         std::move(action), enabled);
}

std::vector<TriggerManager::Trigger> TriggerManager::getTriggersByType(TriggerType type) {
    std::vector<Trigger> result;
    for (auto& trigger : triggers_) {
        if (trigger.type == type) {
            result.push_back(trigger);
        }
    }
    return result;
}

void TriggerManager::evaluate(const DeltaQ& dq, const QTA& qta) const {
    for (const auto& trigger : triggers_) {
        if (trigger.enabled && trigger.condition(dq, qta)) {
            trigger.action(dq, qta);
        }
    }
}

void TriggerManager::removeTriggersByType(TriggerType type) {
    triggers_.erase(
        std::remove_if(triggers_.begin(), triggers_.end(),
            [type](const auto& trigger) { return trigger.type == type; }),
        triggers_.end()
    );
}

void TriggerManager::clearAllTriggers() {
    triggers_.clear();
}

void TriggerManager::setTriggersEnabled(TriggerType type, bool enabled) {
    for (auto& trigger : triggers_) {
        if (trigger.type == type) {
            trigger.enabled = enabled;
        }
    }
}
std::vector<TriggerManager::Trigger> TriggerManager::getAllTriggers() const {
    std::vector<Trigger> result;
    for (const auto& trigger : triggers_) {
        result.push_back(trigger);
    }
    return result;
}
