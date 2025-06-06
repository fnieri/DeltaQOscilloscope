#include "Triggers.h"

namespace TriggerDefs
{
namespace Conditions
{
    Condition SampleLimit(int maxSamples)
    {
        return [maxSamples](const DeltaQ &dq, const QTA &) { return dq.getTotalSamples() > maxSamples; };
    }

    Condition QTABounds()
    {
        return [](const DeltaQ &dq, const QTA &qta) {
            const QTA &dqQta = dq.getQTA();
            return dqQta.perc_25 > qta.perc_25 || dqQta.perc_50 > qta.perc_50 || dqQta.perc_75 > qta.perc_75 || dqQta.cdfMax < qta.cdfMax;
        };
    }

    Condition FailureRate(double threshold)
    {
        return [threshold](const DeltaQ &dq, const QTA &) { return dq.getQTA().cdfMax < threshold; };
    }
}

namespace Actions
{
    Action LogToConsole(const std::string &message)
    {
        return [message](const DeltaQ &, const QTA &, std::uint64_t) { std::cout << "TRIGGER: " << message << "\n"; };
    }

    Action notify()
    {
        return [](const DeltaQ &dq, const QTA &qta, std::uint64_t) { };
    }

    Action SaveSnapshot(const std::string &filename)
    {
        return [](const DeltaQ &dq, const QTA &, std::uint64_t) { };
    }
}
}
