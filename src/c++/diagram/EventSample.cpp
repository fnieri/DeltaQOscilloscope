#include "EventSample.h"

// Implement operator<< for EventSample
std::ostream& operator<<(std::ostream& os, const EventSample& sample) {
    return os << "ID: " << sample.id << ", Start time: " << sample.startTime;
}

// Implement comparison by start time
bool compareSamplesByStartTime(const EventSample& a, const EventSample& b) {
    return a.startTime <= b.startTime;
}

// Implement comparison by ID
bool compareSamplesById(const EventSample& a, const EventSample& b) {
    return a.id < b.id;
}

// Specialization of std::hash for EventSample
namespace std {
    std::size_t hash<EventSample>::operator()(const EventSample& sample) const noexcept {
        return std::hash<int>()(sample.id);
    }
}
