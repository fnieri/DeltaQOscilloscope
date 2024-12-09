/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing an event in a DeltaQ system with associated samples
 */
#pragma once

#include "DiagramComponent.h"
#include "EventSample.h"
#include <memory>
#include <vector>

class Event final : public DiagramComponent
{
    std::vector<EventSample> samples;

public:
    explicit Event(const std::string &name);
    void addSample(const EventSample &sample);
    /**
     * Get all samples after a certain time
     */
    std::vector<EventSample> getSamplesAfter(float startTime) const;
    std::vector<EventSample> getSamples() const;

    DeltaQ calculateDeltaQ(const System &system, const DeltaQ &deltaQ) override;
};
