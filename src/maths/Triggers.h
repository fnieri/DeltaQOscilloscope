#pragma once

#include "DeltaQ.h"
#include "QTA.h"
#include "TriggerTypes.h"
#include <functional>
#include <iostream>
#include <string>

namespace TriggerDefs
{
using Condition = std::function<bool(const DeltaQ &, const QTA &)>;
using Action = std::function<void(const DeltaQ &, const QTA &, std::uint64_t)>;

namespace Conditions
{
    Condition SampleLimit(int maxSamples);
    Condition QTABounds();
    Condition FailureRate(double threshold);
}

namespace Actions
{
    Action LogToConsole(const std::string &message);
    Action notify();
    Action SaveSnapshot(const std::string &filename);
}
}
