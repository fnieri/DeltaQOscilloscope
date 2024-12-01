/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing an event in a DeltaQ system with associated samples
 */
#pragma once

#include <vector>
#include "DiagramComponent.h"
#include "EventSample.h"


class Event final : virtual public DiagramComponent {
    std::vector<EventSample> samples;
    std::shared_ptr<DiagramComponent> next;
public:
    Event();
    void addSample(const EventSample& sample);
    /**
     * Get all samples after a certain time 
     */
    std::vector<EventSample> getSamplesAfter(float startTime) const;
    std::vector<EventSample> getSamples() const;
    
    DeltaQ calculateDeltaQ(const System& system) override;
};