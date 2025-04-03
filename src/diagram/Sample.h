#pragma once

#include <cstdint>

enum Status { SUCCESS, TIMEDOUT, FAILED };

struct Sample {
    std::uint64_t startTime;
    std::uint64_t endTime;
    double long elapsedTime;
    Status status;
};
