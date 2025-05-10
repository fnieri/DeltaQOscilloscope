
#include "DQPlotController.h"
#include "../Application.h"
#include "../maths/DeltaQOperations.h"
#include <QMetaObject>
#include <QVector>
#include <QtConcurrent>
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

bool DQPlotController::isEmptyAfterReset()
{
    auto system = Application::getInstance().getSystem();

    std::lock_guard<std::mutex> lock(resetMutex);

    for (auto it = outcomes.begin(); it != outcomes.end();) {
        if (!system->hasOutcome(it->first)) {
            removeComponent(it->first);
            it = outcomes.begin();
        } else {
            ++it;
        }
    }

    for (auto it = probes.begin(); it != probes.end();) {
        if (!system->hasProbe(it->first)) {
            removeComponent(it->first);
            it = probes.begin();
        } else {
            ++it;
        }
    }
    return (outcomes.empty() && probes.empty());
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

void DQPlotController::addComponent(const std::string &name, bool isProbe)
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

        ProbeAllSeries series = {.probeS = probeSeries,
            .calculatedProbeS = calculatedProbeSeries,
            .lowerBoundS = lowerBoundSeries,
            .upperBoundS = upperBoundSeries,
            .meanS = meanSeries,
            .qtaS = qtaSeries};

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
    std::lock_guard<std::mutex> lock(updateMutex);
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

double DQPlotController::updateOutcome(QLineSeries *series, const std::shared_ptr<Outcome> &outcome, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    auto ret = QtConcurrent::run([=]() {
        DeltaQ deltaQ = outcome->getObservableDeltaQ(timeLowerBound, timeUpperBound);
        int size = deltaQ.getBins();
        double binWidth = deltaQ.getBinWidth();

        std::vector<std::pair<double, double>> data;
        data.reserve(size);

        for (int i = 0; i < size; ++i) {
            data.emplace_back(binWidth * (i + 1), deltaQ.cdfAt(i));
        }

        QMetaObject::invokeMethod(plot, [=]() { plot->updateSeries(series, data); }, Qt::QueuedConnection);
    });

    // Return the max delay
    return outcome->getMaxDelay();
}
void DQPlotController::updateProbe(ProbeAllSeries probeAllSeries, std::shared_ptr<Probe> &probe, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    using namespace std::chrono;

    auto ret = QtConcurrent::run([=]() {
        //      auto computeStart = high_resolution_clock::now();

        DeltaQ observedDeltaQ = probe->getObservableDeltaQ(timeLowerBound, timeUpperBound);
        DeltaQ probeCalculatedDeltaQ = probe->getProbeDeltaQ(timeLowerBound, timeUpperBound);
        std::vector<Bound> bounds = probe->getBounds();
        auto qta = probe->getQTA();
        double maxDelay = probe->getMaxDelay();

        // --- Prepare data ---
        std::vector<std::pair<double, double>> probeData;
        std::vector<std::pair<double, double>> calculatedProbeData;
        std::vector<std::pair<double, double>> lowerBoundData;
        std::vector<std::pair<double, double>> upperBoundData;
        std::vector<std::pair<double, double>> meanData;
        std::vector<std::pair<double, double>> qtaData;

        // Prepare observedDeltaQ data
        int observedBins = observedDeltaQ.getBins();
        double observedBinWidth = observedDeltaQ.getBinWidth();
        probeData.reserve(observedBins);
        for (int i = 0; i < observedBins; ++i) {
            probeData.emplace_back(observedBinWidth * (i + 1), observedDeltaQ.cdfAt(i));
        }

        // Prepare calculatedDeltaQ data
        int calculatedBins = probeCalculatedDeltaQ.getBins();
        double calculatedBinWidth = probeCalculatedDeltaQ.getBinWidth();
        calculatedProbeData.reserve(calculatedBins);
        for (int i = 0; i < calculatedBins; ++i) {
            calculatedProbeData.emplace_back(calculatedBinWidth * (i + 1), probeCalculatedDeltaQ.cdfAt(i));
        }

        // Prepare bounds
        int boundsSize = static_cast<int>(bounds.size());
        lowerBoundData.reserve(boundsSize);
        upperBoundData.reserve(boundsSize);
        meanData.reserve(boundsSize);
        for (int i = 0; i < boundsSize; ++i) {
            double x = observedBinWidth * (i + 1);
            lowerBoundData.emplace_back(x, bounds[i].lowerBound);
            upperBoundData.emplace_back(x, bounds[i].upperBound);
            meanData.emplace_back(x, bounds[i].mean);
        }

        // QTA line plot
        qtaData.reserve(8);
        qtaData.emplace_back(qta.perc_25, 0);
        qtaData.emplace_back(qta.perc_25, 0.25);
        qtaData.emplace_back(qta.perc_50, 0.25);
        qtaData.emplace_back(qta.perc_50, 0.5);
        qtaData.emplace_back(qta.perc_75, 0.5);
        qtaData.emplace_back(qta.perc_75, 0.75);
        qtaData.emplace_back(maxDelay, 0.75);
        qtaData.emplace_back(maxDelay, qta.cdfMax);

        //    auto computeEnd = high_resolution_clock::now();
        //  qDebug() << "Computation took" << duration_cast<microseconds>(computeEnd - computeStart).count() << "µs";

        // --- Push results back to GUI thread ---

        QMetaObject::invokeMethod(
            plot,
            [=]() {
                //        auto guiStart = high_resolution_clock::now();

                plot->setUpdatesEnabled(false);

                // Efficiently replace series data
                plot->updateSeries(probeAllSeries.probeS, probeData);
                plot->updateSeries(probeAllSeries.calculatedProbeS, calculatedProbeData);
                plot->updateSeries(probeAllSeries.lowerBoundS, lowerBoundData);
                plot->updateSeries(probeAllSeries.upperBoundS, upperBoundData);
                plot->updateSeries(probeAllSeries.meanS, meanData);
                plot->updateSeries(probeAllSeries.qtaS, qtaData);

                plot->setUpdatesEnabled(true);

                //    auto guiEnd = high_resolution_clock::now();
                //      qDebug() << "GUI update took" << duration_cast<microseconds>(guiEnd - guiStart).count() << "µs";
            },
            Qt::QueuedConnection);
    });
}
