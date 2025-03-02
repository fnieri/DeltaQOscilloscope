
#include "DQPlotController.h"
#include "../maths/DeltaQOperations.h"

#include <iostream>
DQPlotController::DQPlotController(std::shared_ptr<System> system, DeltaQPlot *plot, const std::vector<std::string> &selectedItems)
    : system(system)
    , plot(plot)
{
    for (const auto &name : selectedItems) {
        if (system->hasProbe(name)) {
            addComponent(name, true);
        } else {
            addComponent(name, false);
        }
    }
}

bool DQPlotController::containsComponent(std::string name)
{
    return ((outcomes.find(name) != outcomes.end()) || (probes.find(name) != probes.end()));
}

void DQPlotController::editPlot(const std::vector<std::string> &selectedItems)
{
    std::vector<std::string> existingItems = getComponents();

    // Remove unselected items
    for (const auto &name : existingItems) {
        if (std::find(selectedItems.begin(), selectedItems.end(), name) == selectedItems.end()) {
            removeComponent(name);
        }
    }

    // Add new selections
    for (const auto &name : selectedItems) {
        if (!containsComponent(name)) {
            if (system->hasProbe(name)) {
                addComponent(name, true);
            } else {
                addComponent(name, false);
            }
        }
    }
}

void DQPlotController::addComponent(std::string name, bool isProbe)
{
    auto series = new QLineSeries();

    if (!isProbe) {
        outcomes[name] = {series, system->getOutcome(name)};
    } else {
        probes[name] = {series, system->getProbe(name)};
    }

    plot->addSeries(series, name);
}

std::vector<std::string> DQPlotController::getComponents()
{
    std::vector<std::string> components;
    components.reserve(probes.size() + outcomes.size());

    for (const auto &kv : probes) {
        components.push_back(kv.first);
    }
    for (const auto &kv : outcomes) {
        components.push_back(kv.first);
    }

    return components;
}

void DQPlotController::removeComponent(std::string &&name)
{
    if (outcomes.count(name)) {
        plot->removeSeries(outcomes[name].first);
        outcomes.erase(name);
        return;
    }
    if (probes.count(name)) {
        plot->removeSeries(probes[name].first);
        probes.erase(name);
    }
}

void DQPlotController::removeComponent(const std::string &name)
{
    if (outcomes.count(name)) {
        plot->removeSeries(outcomes[name].first);
        outcomes.erase(name);
        return;
    }
    if (probes.count(name)) {
        plot->removeSeries(probes[name].first);
        probes.erase(name);
    }
}

void DQPlotController::update()
{
    double binWidth = system->getBinWidth();
    for (auto &[name, seriesOutcome] : outcomes) {
        updateComponent(seriesOutcome.first, seriesOutcome.second, binWidth);
    }

    for (auto &[name, seriesProbe] : probes) {
        updateComponent(seriesProbe.first, seriesProbe.second, binWidth);
    }
}

void DQPlotController::updateComponent(QLineSeries *series, std::shared_ptr<Probe> component, double binWidth)
{
    std::vector<std::pair<double, double>> data;
    DeltaQ deltaQ = component->getDeltaQ(binWidth);
    int size = deltaQ.getSize();

    for (int i = 0; i < size; i++) {
        data.push_back({binWidth * (i + 1), deltaQ.cdfAt(i)});
    }

    plot->updateSeries(series, data, binWidth * size);
}
