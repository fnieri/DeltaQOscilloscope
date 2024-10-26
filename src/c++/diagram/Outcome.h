/**
 * @author Francesco Nieri
 * @date 25/10/2024
 * Class representing an outcome O_n in a system
 */

#include "Event.h"
#include "DiagramComponent.h"
#include <memory>

class Outcome : virtual public DiagramComponent {
private:
    
    std::shared_ptr<Event> startEvent;
    std::shared_ptr<Event> endEvent;

public:
    Outcome(Event& startEvent, Event& endEvent);
    std::shared_ptr<Event> getStartEvent();
    std::shared_ptr<Event> getEndEvent();
    
    /**
     * Get the samples associated to an outcome
     * //TODO this is just a prototype, it will need to be adjusted to fit time constraints
     */
    std::unordered_map<int, float> getOutcomeSamples();
};