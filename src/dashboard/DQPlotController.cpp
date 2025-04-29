
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

    for (const auto &name : existingItems) {
        if (std::find(selectedItems.begin(), selectedItems.end(), name) == selectedItems.end()) {
            removeComponent(name);
        }
    }
    auto system = Application::getInstance().getSystem();

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
        std::string probeSeriesName = name + " observed";

        auto calculatedProbeSeries = new QLineSeries();
        std::string calculatedProbeSeriesName = name + " calculated";

        auto meanSeries = new QLineSeries();
        std::string meanSeriesName = name + " mean";

        auto qtaSeries = new QLineSeries();
        std::string qtaSeriesName = name + " qta";

        ProbeAllSeries series
            = {.probeS = probeSeries,
            .calculatedProbeS = calculatedProbeSeries,
            .lowerBoundS = lowerBoundSeries,
            .upperBoundS = upperBoundSeries,
            .meanS = meanSeries,
            .qtaS = qtaSeries
            };

        probes[name] = {series, system->getProbe(name)};

        plot->addSeries(probeSeries, probeSeriesName);
        plot->addSeries(calculatedProbeSeries, calculatedProbeSeriesName);
        plot->addSeries(lowerBoundSeries, lowerBoundName);
        plot->addSeries(upperBoundSeries, upperBoundName);
        plot->addSeries(meanSeries, meanSeriesName);
        plot->addSeries(qtaSeries, qtaSeriesName);
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
        plot->removeSeries(allSeries.meanS);
        plot->removeSeries(allSeries.qtaS);
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
        plot->removeSeries(allSeries.meanS);
        plot->removeSeries(allSeries.qtaS);

        probes.erase(name);
    }
}

void DQPlotController::update(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    double outcomeMax = 0;
    for (auto &[name, seriesOutcome] : outcomes) {
        double outcomeRange = updateOutcome(seriesOutcome.first, seriesOutcome.second, timeLowerBound, timeUpperBound);
        if (outcomeRange > outcomeMax)
            outcomeMax = outcomeRange;
        plot->updateXRange(outcomeMax);
    }

    for (auto &[name, seriesProbe] : probes) {
        updateProbe(probes[name].first, probes[name].second, timeLowerBound, timeUpperBound);
    }
}

double DQPlotController::updateOutcome(QLineSeries *series, const std::shared_ptr<Outcome>& outcome, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::vector<std::pair<double, double>> data;

    DeltaQ deltaQ = outcome->getObservableDeltaQ(timeLowerBound, timeUpperBound);
    int size = deltaQ.getBins();
    double binWidth = deltaQ.getBinWidth();

    for (int i = 0; i < size; i++) {
        data.emplace_back(binWidth * (i + 1), deltaQ.cdfAt(i));
    }

    plot->updateSeries(series, data);
    return outcome->getMaxDelay();
}

void DQPlotController::updateProbe(ProbeAllSeries probeAllSeries, std::shared_ptr<Probe> probe, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    // Plot observed DeltaQ

    DeltaQ observedDeltaQ = probe->getObservableDeltaQ(timeLowerBound, timeUpperBound);
    int observedBins = observedDeltaQ.getBins();
    double observedBinWidth = observedDeltaQ.getBinWidth();
    std::vector<std::pair<double, double>> probeData;

    for (int i = 0; i < observedBins; i++) {
        probeData.emplace_back(observedBinWidth * (i + 1), observedDeltaQ.cdfAt(i));
    }
    plot->updateSeries(probeAllSeries.probeS, probeData);

    //Update calculated deltaQ

    DeltaQ probeCalculatedDeltaQ = probe->getProbeDeltaQ(timeLowerBound, timeUpperBound);

    int calculatedBins = probeCalculatedDeltaQ.getBins();
    auto calculatedBinWidth = probeCalculatedDeltaQ.getBinWidth();
    std::vector<std::pair<double, double>> calculatedProbeData;

    for (int i = 0; i < calculatedBins; i++) {
        calculatedProbeData.emplace_back(calculatedBinWidth * (i + 1), probeCalculatedDeltaQ.cdfAt(i));
    }

    plot->updateSeries(probeAllSeries.calculatedProbeS, calculatedProbeData);


    // Update confidence bounds

    std::vector<Bound> bounds = probe->getBounds();

    std::vector<std::pair<double, double>> lowerBoundData;
    std::vector<std::pair<double, double>> upperBoundData;
    std::vector<std::pair<double, double>> meanData;

    for (int i = 0; i < bounds.size(); i++) {
        double x = observedBinWidth * (i + 1);
        lowerBoundData.emplace_back(x, bounds[i].lowerBound);
        upperBoundData.emplace_back(x, bounds[i].upperBound);
        meanData.emplace_back(x, bounds[i].mean);
    }

    plot->updateSeries(probeAllSeries.lowerBoundS, lowerBoundData);
    plot->updateSeries(probeAllSeries.upperBoundS, upperBoundData);
    plot->updateSeries(probeAllSeries.meanS, meanData);

    // Plot QTA

    auto qta = probe->getQTA();
    std::vector<std::pair<double, double>> qtaData;
    qtaData.emplace_back(qta.perc_25, 0);
    qtaData.emplace_back(qta.perc_25, 0.25);
    qtaData.emplace_back(qta.perc_50, 0.25);
    qtaData.emplace_back(qta.perc_50, 0.5);
    qtaData.emplace_back(qta.perc_75, 0.5);
    qtaData.emplace_back(qta.perc_75, 0.75);
    qtaData.emplace_back(probe->getMaxDelay(), 0.75);
    qtaData.emplace_back(probe->getMaxDelay(), qta.cdfMax);

    plot->updateSeries(probeAllSeries.qtaS, qtaData);
}
