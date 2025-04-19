
#include "DQPlotController.h"
#include "../Application.h"
#include "../maths/DeltaQOperations.h"
#include <algorithm>
#include <qlineseries.h>
DQPlotController::DQPlotController(DeltaQPlot *plot, const std::vector<std::string> &selectedItems)
    : plot(plot)
{
    auto system = Application::getInstance().getSystem();
    for (const auto &name : selectedItems) {
        if (system->hasProbe(name)) {
            addComponent(name, true);
        } else {
            addComponent(name, false);
        }
    }
}

DQPlotController::~DQPlotController()
{
    outcomes.clear();
    probes.clear();
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
    auto system = Application::getInstance().getSystem();
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
    auto system = Application::getInstance().getSystem();
    if (!isProbe) {
        auto series = new QLineSeries();
        outcomes[name] = {series, system->getOutcome(name)};

        plot->addSeries(series, name);
    } else {

        auto lowerBoundSeries = new QLineSeries();
        std::string lowerBoundName = name + " lower bound";

        auto upperBoundSeries = new QLineSeries();
        std::string upperBoundName = name + " upper bound";

        auto probeSeries = new QLineSeries();
        std::string probeSeriesName = name + " time series";

        auto calculatedProbeSeries = new QLineSeries();
        std::string calculatedProbeSeriesName = name + " calculated DQ";
        ProbeAllSeries series
            = {.probeS = probeSeries, .calculatedProbeS = calculatedProbeSeries, .lowerBoundS = lowerBoundSeries, .upperBoundS = upperBoundSeries};

        probes[name] = {series, system->getProbe(name)};

        plot->addSeries(probeSeries, probeSeriesName);
        plot->addSeries(calculatedProbeSeries, calculatedProbeSeriesName);
        plot->addSeries(lowerBoundSeries, lowerBoundName);
        plot->addSeries(upperBoundSeries, upperBoundName);
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
        ProbeAllSeries allSeries = probes[name].first;
        plot->removeSeries(allSeries.lowerBoundS);
        plot->removeSeries(allSeries.upperBoundS);
        plot->removeSeries(allSeries.calculatedProbeS);
        plot->removeSeries(allSeries.probeS);
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
        ProbeAllSeries allSeries = probes[name].first;
        plot->removeSeries(allSeries.lowerBoundS);
        plot->removeSeries(allSeries.upperBoundS);
        plot->removeSeries(allSeries.calculatedProbeS);
        plot->removeSeries(allSeries.probeS);
        probes.erase(name);
    }
}

void DQPlotController::update(double binWidth, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    for (auto &[name, seriesOutcome] : outcomes) {
        updateOutcome(seriesOutcome.first, seriesOutcome.second, binWidth, timeLowerBound, timeUpperBound);
    }

    for (auto &[name, seriesProbe] : probes) {
        updateProbe(probes[name].first, probes[name].second, binWidth, timeLowerBound, timeUpperBound);
    }
}

void DQPlotController::updateOutcome(QLineSeries *series, std::shared_ptr<Outcome> outcome, double binWidth, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::vector<std::pair<double, double>> data;
    DeltaQ deltaQ = outcome->getDeltaQ(binWidth, timeLowerBound, timeUpperBound);
    int size = deltaQ.getSize();

    for (int i = 0; i < size; i++) {
        data.push_back({binWidth * (i + 1), deltaQ.cdfAt(i)});
    }

    plot->updateSeries(series, data, binWidth * size);
}

void DQPlotController::updateProbe(
    ProbeAllSeries probeAllSeries, std::shared_ptr<Probe> probe, double binWidth, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    ProbeDeltaQ probeDeltaQs = probe->getDeltaQ(binWidth, timeLowerBound, timeUpperBound);
    DeltaQ probeDeltaQ = probeDeltaQs.probeDeltaQ;
    int size = probeDeltaQ.getSize();
    double probeBinWidth = probeDeltaQ.getBinWidth();
    std::vector<std::pair<double, double>> probeData;
    for (int i = 0; i < size; i++) {
        probeData.push_back({probeBinWidth * (i + 1), probeDeltaQ.cdfAt(i)});
    }

    plot->updateSeries(probeAllSeries.probeS, probeData, probeBinWidth * size);

    DeltaQ calculatedProbeDeltaQ = probeDeltaQs.calculatedProbeDeltaQ;
    int calculatedSize = calculatedProbeDeltaQ.getSize();
    std::vector<std::pair<double, double>> calculatedProbeData;
    for (int i = 0; i < calculatedSize; i++) {
        calculatedProbeData.push_back({binWidth * (i + 1), calculatedProbeDeltaQ.cdfAt(i)});
    }
    plot->updateSeries(probeAllSeries.calculatedProbeS, calculatedProbeData, binWidth * calculatedSize);

    std::vector<Bound> bounds = probeDeltaQs.bounds;
    std::vector<std::pair<double, double>> lowerBoundData;
    std::vector<std::pair<double, double>> upperBoundData;
    for (int i = 0; i < bounds.size(); i++) {
        lowerBoundData.push_back({binWidth * (i + 1), bounds[i].lowerBound});
        upperBoundData.push_back({binWidth * (i + 1), bounds[i].upperBound});
    }
    plot->updateSeries(probeAllSeries.lowerBoundS, lowerBoundData, binWidth * size);
    plot->updateSeries(probeAllSeries.upperBoundS, upperBoundData, binWidth * size);
}
