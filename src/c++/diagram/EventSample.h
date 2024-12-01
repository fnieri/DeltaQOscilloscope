#ifndef EVENT_SAMPLE_H
#define EVENT_SAMPLE_H

#include <iostream>
#include <functional>

// Structure to represent an event sample
struct EventSample {
    int id;
    double startTime; // Use double for simplicity; consider std::chrono for DateTime-like precision

    // Overload operator<< for printing EventSample objects
    friend std::ostream& operator<<(std::ostream& os, const EventSample& sample);
};

// Comparison functions for EventSample
bool compareSamplesByStartTime(const EventSample& a, const EventSample& b);
bool compareSamplesById(const EventSample& a, const EventSample& b);

// Specialization of std::hash for EventSample
template<>
struct std::hash<EventSample> {
    std::size_t operator()(const EventSample& sample) const noexcept;
};

#endif // EVENT_SAMPLE_H
