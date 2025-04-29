#include "Triggers.h"

namespace TriggerDefs {
    namespace Conditions {
        Condition SampleLimit(int maxSamples) {
            return [maxSamples](const DeltaQ& dq, const QTA&) {
                return dq.getTotalSamples() > maxSamples;
            };
        }

        Condition QTABounds() {
            return [](const DeltaQ& dq, const QTA& qta) {
                const QTA& dqQta = dq.getQTA();
                return dqQta.perc_25 > qta.perc_25 ||
                       dqQta.perc_50 > qta.perc_50 ||
                       dqQta.perc_75 > qta.perc_75;
            };
        }

        Condition FailureRate(double threshold) {
            return [threshold](const DeltaQ& dq, const QTA&) {
                return dq.getQTA().cdfMax < threshold;
            };
        }
    }

    namespace Actions {
        Action LogToConsole(const std::string& message) {
            return [message](const DeltaQ&, const QTA&) {
                std::cout << "TRIGGER: " << message << std::endl;
            };
        }

        Action notify() {
            return [](const DeltaQ& dq, const QTA& qta) {
            };
        }

        Action SaveSnapshot(const std::string& filename) {
            return [](const DeltaQ& dq, const QTA&) {
            };
        }
    }
}