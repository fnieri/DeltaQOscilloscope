/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing an event in a DeltaQ system with associated samples
 */

#include "Event.h"
#include <utility>

#include "../maths/DeltaQOperations.h"

Event::Event(const std::string name)
    : DiagramComponent {name}
{
}

DeltaQ Event::calculateDeltaQ(const System &system, const DeltaQ &deltaQ)
{
    /*
    // Case where this is the first event in the system
    if (deltaQ == DeltaQ()) {
        return next->calculateDeltaQ(system, deltaQ);
    }
    // General case
    if (next) {
        return convolve(deltaQ, next->calculateDeltaQ(system, deltaQ));
    }
    // Last event in system
    */
    return DeltaQ();
}

void Event::addSample(const EventSample &sample)
{
    samples.push_back(sample);
}

std::vector<EventSample> Event::getSamples() const
{
    return samples;
}
