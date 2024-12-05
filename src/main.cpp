#include "diagram/Event.h"
#include "diagram/Outcome.h"
#include <QApplication>
#include <chrono>
#include <iostream>
#include <random>
#include <thread>

// Helper function to get current time in milliseconds
double getCurrentTimeMs()
{
    auto now = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch());
    return static_cast<double>(duration.count());
}

// Generate a sample and add it to the Event
void generateSample(double delay, Event &event, int id)
{
    auto startTime = getCurrentTimeMs();
    std::this_thread::sleep_for(std::chrono::duration<double>(delay));

    EventSample sample {id, startTime};
    event.addSample(sample);
}

// Helper function to create random samples for an event
void generateEventSamples(Event &event, int numSamples, double minDelay, double maxDelay)
{
    std::default_random_engine generator(static_cast<unsigned>(time(nullptr)));
    std::uniform_real_distribution<double> distribution(minDelay, maxDelay);

    for (int i = 0; i < numSamples; ++i) {
        double delay = distribution(generator);
        generateSample(delay, event, i);
    }
}

/*
int main() {
    auto event1 = std::make_shared<Event>("start");
    auto event2 = std::make_shared<Event>("middle");
    auto event3 = std::make_shared<Event>("end");
    auto system = System();

    system.addComponent(event1);
    system.addComponent(event2);
    system.addComponent(event3);
    system.setFirstComponent("start");

    auto outcome1 = std::make_shared<Outcome>("outcome1", event1, event2);
    auto outcome2 = std::make_shared<Outcome>("outcome2", event2, event3);

    system.addComponent(outcome1);
    system.addComponent(outcome2);
    event1->setNext(outcome1);
    event2->setNext(outcome2);
    // Generate samples for start, middle, and end events
    std::cout << "Generating samples for events..." << std::endl;
    generateEventSamples(*event1, 10, 0.1, 0.5); // 5 samples, delays between 0.1s to 0.5s
    generateEventSamples(*event2, 10, 0.2, 0.7); // 5 samples, delays between 0.2s to 0.7s
    generateEventSamples(*event3, 10, 0.3, 0.8); // 5 samples, delays between 0.3s to 0.8s

    // Retrieve the samples and print them
    std::cout << "Event samples:" << std::endl;

    for (const auto& sample : event1->getSamples()) {
        std::cout << "Event1 Sample ID: " << sample.id
                  << ", Start Time: " << sample.startTime << std::endl;
    }
    for (const auto& sample : event2->getSamples()) {
        std::cout << "Event2 Sample ID: " << sample.id
                  << ", Start Time: " << sample.startTime << std::endl;
    }
    for (const auto& sample : event3->getSamples()) {
        std::cout << "Event3 Sample ID: " << sample.id
                  << ", Start Time: " << sample.startTime << std::endl;
    }

    // Perform convolution (example only; replace with actual DeltaQ implementation)
    std::cout << "Performing convolution..." << std::endl;

    DeltaQ final = system.calculateDeltaQ();
    std::cout << "A: " << outcome1->getDeltaQ(system).toString() << std::endl;

    std::cout << "B: " << outcome2->getDeltaQ(system).toString() << std::endl;
    // Print results (Assuming DeltaQ has a toString or similar method)
    std::cout << "Final DeltaQ after convolution: " << final.toString() << std::endl;

    return 0;
}
*/
