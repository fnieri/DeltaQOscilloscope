#include <vector>

struct EventSample {
    int id;
    float startTime;

    bool operator<(const EventSample& other) const {
        return startTime < other.startTime;
    }
}


class Event : virtual public Event {
private:
    std::vector<EventSample> samples;   

public:
    void addSample(const EventSample& sample);
    std::vector<EventSample> getSamplesAfter(float startTime) const;
}