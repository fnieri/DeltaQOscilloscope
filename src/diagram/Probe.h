#pragma once
#include <string>
#include <vector>

#include "../maths/ConfidenceInterval.h"
#include "../maths/DeltaQ.h"
#include "Observable.h"
#include <map>
#include <memory>
#include <mutex>

/**
 * @class Class representing a probe containing causal link
 */

class Probe : public Observable
{
    std::vector<std::shared_ptr<Observable>> causalLinks;

    std::mutex calcMutex;

    ConfidenceInterval calculatedInterval;
    std::deque<DeltaQ> calculatedDeltaQHistory;

public:
    Probe(const std::string &name);

    /**
     * @brief Construct a probe with its causal links
     */
    Probe(const std::string &name, std::vector<std::shared_ptr<Observable>>);

    ~Probe();
    /**
     * @brief calculate a Calculated deltaQ with bounds timeLowerBound, timeUpperBound
     * @param timeLowerBound
     * @param timeUpperBound
     * @return calculated DeltaQ
     */
    DeltaQ calculateCalculatedDeltaQ(uint64_t timeLowerBound, uint64_t timeUpperBound);

    /**
     * @brief Get the representation of a calculated DeltaQ for plotting
     * @return the representation of a calculated DeltaQ
     */
    DeltaQRepr getCalculatedDeltaQRepr(uint64_t, uint64_t);

    std::vector<Bound> getBounds() const;

    std::vector<Bound> getObservedBounds() const;

    std::vector<Bound> getCalculatedBounds() const;


    void setCausalLinks(std::vector<std::shared_ptr<Observable>> newCausalLinks)
    {
        causalLinks = newCausalLinks;
    }

    std::vector<std::shared_ptr<Observable>> getCausalLinks()
    {
        return causalLinks;
    }
};
