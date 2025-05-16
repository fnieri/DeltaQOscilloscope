
#include "DQPlotController.h"
#include "../Application.h"
#include "Point.h"
#include <QMetaObject>
#include <QVector>
#include <QtConcurrent>
#include <algorithm>
#include <qlineseries.h>
using namespace std::chrono;
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
        addOutcomeSeries(name);
    } else {
        addProbeSeries(name);
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

QLineSeries *DQPlotController::createAndAddLineSeries(const std::string &legendName)
{
    auto series = new QLineSeries();
    plot->addSeries(series, legendName);
    return series;
}

void DQPlotController::addOutcomeSeries(const std::string &name)
{
    auto system = Application::getInstance().getSystem();

    auto lowerBoundSeries = createAndAddLineSeries(name + " lower bound");
    auto upperBoundSeries = createAndAddLineSeries(name + " upper bound");
    auto outcomeSeries = createAndAddLineSeries(name + " observed");
    auto meanSeries = createAndAddLineSeries(name + " mean");
    auto qtaSeries = createAndAddLineSeries(name + " qta");

    OutcomeSeries series
        = {.outcomeS = outcomeSeries, .lowerBoundS = lowerBoundSeries, .upperBoundS = upperBoundSeries, .meanS = meanSeries, .qtaS = qtaSeries};

    outcomes[name] = {series, system->getOutcome(name)};
}

void DQPlotController::addProbeSeries(const std::string &name)
{
    auto system = Application::getInstance().getSystem();

    auto obsLowerBoundSeries = createAndAddLineSeries(name + " lower bound");
    auto obsUpperBoundSeries = createAndAddLineSeries(name + " upper bound");
    auto obsSeries = createAndAddLineSeries(name + " observed");
    auto obsMeanSeries = createAndAddLineSeries(name + " mean");

    auto qtaSeries = createAndAddLineSeries(name + " qta");

    auto calcSeries = createAndAddLineSeries(name + " calculated");
    auto calcLowerBoundSeries = createAndAddLineSeries(name + " lower bound");
    auto calcUpperBoundSeries = createAndAddLineSeries(name + " upper bound");
    auto calcMeanSeries = createAndAddLineSeries(name + " mean");

    ProbeAllSeries series = {
        .obsS = obsSeries,
        .obsLowerBoundS = obsLowerBoundSeries,
        .obsUpperBoundS = obsUpperBoundSeries,
        .obsMeanS = obsMeanSeries,
        .calcS = calcSeries,
        .calcLowerBoundS = calcLowerBoundSeries,
        .calcUpperBoundS = calcUpperBoundSeries,
        .calcMeanS = calcMeanSeries,
        .qtaS = qtaSeries,
    };
    probes[name] = {series, system->getProbe(name)};
}

void DQPlotController::removeOutcomeSeries(const std::string &name)
{
    OutcomeSeries series = outcomes[name].first;
    plot->removeSeries(series.outcomeS);
    plot->removeSeries(series.lowerBoundS);
    plot->removeSeries(series.upperBoundS);
    plot->removeSeries(series.meanS);
    plot->removeSeries(series.qtaS);
}

void DQPlotController::removeProbeSeries(const std::string &name)
{
    ProbeAllSeries allSeries = probes[name].first;

    plot->removeSeries(allSeries.obsLowerBoundS);
    plot->removeSeries(allSeries.obsUpperBoundS);
    plot->removeSeries(allSeries.obsS);
    plot->removeSeries(allSeries.obsMeanS);

    plot->removeSeries(allSeries.qtaS);

    plot->removeSeries(allSeries.calcS);
    plot->removeSeries(allSeries.calcLowerBoundS);
    plot->removeSeries(allSeries.calcUpperBoundS);
    plot->removeSeries(allSeries.calcMeanS);
    probes.erase(name);
}

void DQPlotController::removeComponent(const std::string &name)
{
    if (outcomes.count(name)) {
        removeOutcomeSeries(name);
    }
    if (probes.count(name)) {
        removeProbeSeries(name);
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

double DQPlotController::updateOutcome(OutcomeSeries series, const std::shared_ptr<Outcome> &outcome, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    auto ret = QtConcurrent::run([=]() {
        DeltaQRepr repr = outcome->getObservedDeltaQRepr(timeLowerBound, timeUpperBound);
        DeltaQ deltaQ = repr.deltaQ;
        std::vector<Bound> bounds = repr.bounds;
        int size = deltaQ.getBins();
        double binWidth = deltaQ.getBinWidth();

        QVector<QPointF> deltaQData;
        deltaQData.emplace_back(QPointF(0, 0));
        deltaQData.reserve(size);

        QVector<QPointF> lowerBoundData;
        lowerBoundData.emplace_back(QPointF(0, 0));
        lowerBoundData.reserve(size);

        QVector<QPointF> upperBoundData;
        upperBoundData.emplace_back(QPointF(0, 0));
        upperBoundData.reserve(size);

        QVector<QPointF> meanData;
        meanData.emplace_back(QPointF(0, 0));
        meanData.reserve(size);

        QVector<QPointF> qtaData;

        for (int i = 0; i < size; ++i) {
            int x = binWidth * (i + 1);
            deltaQData.emplace_back(QPointF(x, deltaQ.cdfAt(i)));
            lowerBoundData.emplace_back(QPointF(x, bounds[i].lowerBound));
            upperBoundData.emplace_back(QPointF(x, bounds[i].upperBound));
            meanData.emplace_back(QPointF(x, bounds[i].mean));
        }

        QMetaObject::invokeMethod(
            plot,
            [=]() {
                //        auto guiStart = high_resolution_clock::now();

                plot->setUpdatesEnabled(false);

                plot->updateSeries(series.outcomeS, deltaQData);
                plot->updateSeries(series.lowerBoundS, lowerBoundData);
                plot->updateSeries(series.upperBoundS, upperBoundData);
                plot->updateSeries(series.meanS, meanData);
                // plot->updateSeries(series.qtaS, qtaData);

                plot->setUpdatesEnabled(true);

                //    auto guiEnd = high_resolution_clock::now();
                //      qDebug() << "GUI update took" << duration_cast<microseconds>(guiEnd - guiStart).count() << "µs";
            },
            Qt::QueuedConnection);
    });

    // Return the max delay
    return outcome->getMaxDelay();
}
void DQPlotController::updateProbe(ProbeAllSeries probeAllSeries, std::shared_ptr<Probe> &probe, uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    auto ret = QtConcurrent::run([=]() {
        //      auto computeStart = high_resolution_clock::now();
        DeltaQRepr obsRepr = probe->getObservedDeltaQRepr(timeLowerBound, timeUpperBound);
        DeltaQRepr calcRepr = probe->getCalculatedDeltaQRepr(timeLowerBound, timeUpperBound);
        DeltaQ obsDeltaQ = obsRepr.deltaQ;
        std::vector<Bound> obsBounds = obsRepr.bounds;
        DeltaQ calcDeltaQ = calcRepr.deltaQ;
        std::vector<Bound> calcBounds = calcRepr.bounds;

        auto qta = probe->getQTA();
        double maxDelay = probe->getMaxDelay();

        int observedBins = obsDeltaQ.getBins();
        double observedBinWidth = obsDeltaQ.getBinWidth();

        // --- Prepare data ---
        QVector<QPointF> obsDeltaQData;
        obsDeltaQData.emplace_back(QPointF(0, 0));
        obsDeltaQData.reserve(observedBins);

        QVector<QPointF> obsLowerBoundData;
        obsLowerBoundData.emplace_back(QPointF(0, 0));
        obsLowerBoundData.reserve(observedBins);

        QVector<QPointF> obsUpperBoundData;
        obsUpperBoundData.emplace_back(QPointF(0, 0));
        obsUpperBoundData.reserve(observedBins);

        QVector<QPointF> obsMeanData;
        obsMeanData.emplace_back(QPointF(0, 0));
        obsMeanData.reserve(observedBins);

        for (int i = 0; i < observedBins; ++i) {
            double x = observedBinWidth * (i + 1);
            obsDeltaQData.emplace_back(QPointF(x, obsDeltaQ.cdfAt(i)));
            obsLowerBoundData.emplace_back(QPointF(x, obsBounds[i].lowerBound));
            obsUpperBoundData.emplace_back(QPointF(x, obsBounds[i].upperBound));
            obsMeanData.emplace_back(QPointF(x, obsBounds[i].mean));
        }

        QVector<QPointF> calcDeltaQData;
        calcDeltaQData.emplace_back(QPointF(0, 0));
        calcDeltaQData.reserve(observedBins);

        QVector<QPointF> calcLowerBoundData;
        calcLowerBoundData.emplace_back(QPointF(0, 0));
        calcLowerBoundData.reserve(observedBins);

        QVector<QPointF> calcUpperBoundData;
        calcUpperBoundData.emplace_back(QPointF(0, 0));
        calcUpperBoundData.reserve(observedBins);

        QVector<QPointF> calcMeanData;
        calcMeanData.emplace_back(QPointF(0, 0));
        calcMeanData.reserve(observedBins);

        QVector<QPointF> qtaData;

        // Prepare calculatedDeltaQ data
        int calculatedBins = calcDeltaQ.getBins();
        double calculatedBinWidth = calcDeltaQ.getBinWidth();
        for (int i = 0; i < calculatedBins; ++i) {
            double x = observedBinWidth * (i + 1);
            calcDeltaQData.emplace_back(QPointF(x, calcDeltaQ.cdfAt(i)));
            calcLowerBoundData.emplace_back(QPointF(x, calcBounds[i].lowerBound));
            calcUpperBoundData.emplace_back(QPointF(x, calcBounds[i].upperBound));
            calcMeanData.emplace_back(QPointF(x, calcBounds[i].mean));
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

                plot->updateSeries(probeAllSeries.obsS, obsDeltaQData);
                plot->updateSeries(probeAllSeries.obsLowerBoundS, obsLowerBoundData);
                plot->updateSeries(probeAllSeries.obsUpperBoundS, obsUpperBoundData);
                plot->updateSeries(probeAllSeries.obsMeanS, obsMeanData);

                plot->updateSeries(probeAllSeries.calcS, calcDeltaQData);
                plot->updateSeries(probeAllSeries.calcLowerBoundS, calcLowerBoundData);
                plot->updateSeries(probeAllSeries.calcUpperBoundS, calcUpperBoundData);
                plot->updateSeries(probeAllSeries.calcMeanS, calcMeanData);

                plot->updateSeries(probeAllSeries.qtaS, qtaData);

                plot->setUpdatesEnabled(true);

                //    auto guiEnd = high_resolution_clock::now();
                //      qDebug() << "GUI update took" << duration_cast<microseconds>(guiEnd - guiStart).count() << "µs";
            },
            Qt::QueuedConnection);
    });
}
