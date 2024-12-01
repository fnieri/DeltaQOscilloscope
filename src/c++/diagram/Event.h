/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Class representing an event in a DeltaQ system with associated samples
 */

#include <vector>
#include "DiagramComponent.h"
#include "EventSample.cpp"


class Event : virtual public DiagramComponent {
private:
    std::vector<EventSample> samples;   
    std::shared_ptr<DiagramComponent> next;
public:
    void addSample(const EventSample& sample);
    /**
     * Get all samples after a certain time 
     */
    std::vector<EventSample> getSamplesAfter(float startTime) const;
    std::vector<EventSample> getSamples() const;
    
    DeltaQ calculateDeltaQ(const System& system);
};