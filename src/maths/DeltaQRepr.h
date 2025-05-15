#pragma once
#include "ConfidenceInterval.h"
#include "DeltaQ.h"

struct DeltaQRepr {
    std::uint64_t time;
    DeltaQ deltaQ;
    std::vector<Bound> bounds;
};
