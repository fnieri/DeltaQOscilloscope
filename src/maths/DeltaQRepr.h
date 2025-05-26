
#pragma once

#include "ConfidenceInterval.h"
#include "DeltaQ.h"
#include <cstdint>
#include <vector>

/**
 * Class storing a DeltaQ representation for graphical plotting
 */

struct DeltaQRepr {
    std::uint64_t time;
    DeltaQ deltaQ;
    std::vector<Bound> bounds;

    DeltaQRepr()
        : time(0)
        , deltaQ()
        , bounds()
    {
    }

    // DeltaQRepr copy constructor
    DeltaQRepr(const DeltaQRepr &other)
        : time(other.time)
        , deltaQ(other.deltaQ)
        , bounds(other.bounds)
    {
    }

    // DeltaQRepr move semantics
    DeltaQRepr(DeltaQRepr &&other) noexcept
        : time(other.time)
        , deltaQ(std::move(other.deltaQ))
        , bounds(std::move(other.bounds))
    {
    }

    // DeltaQRepr assignment operators
    DeltaQRepr &operator=(const DeltaQRepr &other)
    {
        if (this != &other) {
            time = other.time;
            deltaQ = other.deltaQ;
            bounds = other.bounds;
        }
        return *this;
    }

    DeltaQRepr &operator=(DeltaQRepr &&other) noexcept
    {
        if (this != &other) {
            time = other.time;
            deltaQ = std::move(other.deltaQ);
            bounds = std::move(other.bounds);
        }
        return *this;
    }

    // Constructor with parameters
    DeltaQRepr(std::uint64_t t, const DeltaQ &dq, const std::vector<Bound> &b)
        : time(t)
        , deltaQ(dq)
        , bounds(b)
    {
    }
};
