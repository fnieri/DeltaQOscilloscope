#include <algorithm>
#include <iostream>
#include <vector>
#include <unordered_map>

struct EventSample {
    int id;
    float startTime; // TODO change usage of float as I don't believe this will work after, rather use something like DateTime

    friend std::ostream& operator<<(std::ostream& os, const EventSample& sample) {
        return os << "ID: " << sample.id << ", Start time: " << sample.startTime;
    }
};


bool compareSamplesByStartTime(const EventSample& a, const EventSample& b) {
    return a.startTime <= b.startTime;
}

bool compareSamplesById(const EventSample&a, const EventSample& b) {
    return a.id < b.id;
}

/**
 * Define hash function on EventSample based on sample id
 * @note This is used when comparing two samples to get the eventSample with the same id
 */
namespace std {
    template<>
    struct hash<EventSample> {
        std::size_t operator()(const EventSample& sample) const {
            return std::hash<int>()(sample.id);
        }
    };
}



