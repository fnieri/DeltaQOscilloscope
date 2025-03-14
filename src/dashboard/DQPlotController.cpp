
#include "DQPlotController.h"
#include "../maths/DeltaQOperations.h"

#include <iostream>
#include <qlineseries.h>
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

    if (!isProbe) {

        auto series = new QLineSeries();
        outcomes[name] = {series, system->getOutcome(name)};

        plot->addSeries(series, name);
    } else {
        auto probeSeries = new QLineSeries();
        std::string probeSeriesName = "Probe " + name + "time series";
        auto calculatedProbeSeries = new QLineSeries();
        std::string calculatedProbeSeriesName = "Probe " + name + "calculated DQ";
        probes[name] = {probeSeries, calculatedProbeSeries, system->getProbe(name)};
        plot->addSeries(probeSeries, probeSeriesName);
        plot->addSeries(calculatedProbeSeries, calculatedProbeSeriesName);
    }
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

void DQPlotController::removeComponent(const std::string &name)
{
    if (outcomes.count(name)) {
        plot->removeSeries(outcomes[name].first);
        outcomes.erase(name);
        return;
    }
    if (probes.count(name)) {
        plot->removeSeries(std::get<0>(probes[name]));
        plot->removeSeries(std::get<1>(probes[name]));
        probes.erase(name);
    }
}

void DQPlotController::removeComponent(std::string &&name)
{
    if (outcomes.count(name)) {
        plot->removeSeries(outcomes[name].first);
        outcomes.erase(name);
        return;
    }
    if (probes.count(name)) {
        plot->removeSeries(std::get<0>(probes[name]));
        plot->removeSeries(std::get<1>(probes[name]));
        probes.erase(name);
    }
}

void DQPlotController::update()
{
    double binWidth = system->getBinWidth();
    for (auto &[name, seriesOutcome] : outcomes) {
        updateOutcome(seriesOutcome.first, seriesOutcome.second, binWidth);
    }

    for (auto &[name, seriesProbe] : probes) {
        updateProbe(std::get<0>(seriesProbe), std::get<1>(seriesProbe), std::get<2>(seriesProbe), binWidth);
    }
}

void DQPlotController::updateOutcome(QLineSeries *series, std::shared_ptr<Outcome> outcome, double binWidth)
{
    std::vector<std::pair<double, double>> data;
    DeltaQ deltaQ = outcome->getDeltaQ(binWidth);
    int size = deltaQ.getSize();

    for (int i = 0; i < size; i++) {
        data.push_back({binWidth * (i + 1), deltaQ.cdfAt(i)});
    }

    plot->updateSeries(series, data, binWidth * size);
}

void DQPlotController::updateProbe(QLineSeries *probeSeries, QLineSeries *calculatedProbeSeries, std::shared_ptr<Probe> probe, double binWidth)
{
    ProbeDeltaQ probeDeltaQs = probe->getDeltaQ(binWidth);
    DeltaQ probeDeltaQ = probeDeltaQs.probeDeltaQ;
    int size = probeDeltaQ.getSize();
    double probeBinWidth = probeDeltaQ.getBinWidth();
    std::vector<std::pair<double, double>> probeData;
    std::cout << "probe " << probeDeltaQ.toString() << "\n";
    for (int i = 0; i < size; i++) {
        probeData.push_back({probeBinWidth * (i + 1), probeDeltaQ.cdfAt(i)});
    }

    plot->updateSeries(probeSeries, probeData, probeBinWidth * size);

    DeltaQ calculatedProbeDeltaQ = probeDeltaQs.calculatedProbeDeltaQ;
    std::cout << "calculated " << calculatedProbeDeltaQ.toString() << "\n";
    int calculatedSize = calculatedProbeDeltaQ.getSize();
    std::cout << calculatedSize << " size \n";
    std::vector<std::pair<double, double>> calculatedProbeData;
    for (int i = 0; i < calculatedSize; i++) {
        calculatedProbeData.push_back({binWidth * (i + 1), calculatedProbeDeltaQ.cdfAt(i)});
    }
    plot->updateSeries(calculatedProbeSeries, calculatedProbeData, binWidth * calculatedSize);
}
