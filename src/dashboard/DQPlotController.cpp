#include "DQPlotController.h"
#include "../Application.h"
#include "ColorRegistry.h"
#include <QAreaSeries>
#include <QLineSeries>
#include <QVector>
#include <algorithm>

DQPlotController::DQPlotController(DeltaQPlot *plot, const std::vector<std::string> &selectedItems)
    : plot(plot)
{
    auto system = Application::getInstance().getSystem();
    for (const auto &name : selectedItems)
        addComponent(name, system->hasOutcome(name));
    setTitle();
}

DQPlotController::~DQPlotController()
{
    outcomes.clear();
    probes.clear();
    operators.clear();
}

// -----------------------------------------------------------------------
// Bounds mode toggle
// -----------------------------------------------------------------------

void DQPlotController::applyBoundsModeToOutcome(OutcomeSeries &s)
{
    plot->setSeriesVisible(s.boundsArea,   boundsAsArea);
    plot->setSeriesVisible(s.upperBoundS, !boundsAsArea);
    plot->setSeriesVisible(s.lowerBoundS, !boundsAsArea);
}

void DQPlotController::applyBoundsModeToExpression(ExpressionSeries &s)
{
    plot->setSeriesVisible(s.obsBoundsArea,   boundsAsArea);
    plot->setSeriesVisible(s.obsUpperBoundS, !boundsAsArea);
    plot->setSeriesVisible(s.obsLowerBoundS, !boundsAsArea);
    plot->setSeriesVisible(s.calcBoundsArea,   boundsAsArea);
    plot->setSeriesVisible(s.calcUpperBoundS, !boundsAsArea);
    plot->setSeriesVisible(s.calcLowerBoundS, !boundsAsArea);
}

void DQPlotController::setBoundsAsArea(bool useArea)
{
    boundsAsArea = useArea;
    for (auto &[name, p] : outcomes)  applyBoundsModeToOutcome(p.first);
    for (auto &[name, p] : probes)    applyBoundsModeToExpression(p.first);
    for (auto &[name, p] : operators) applyBoundsModeToExpression(p.first);
}

// -----------------------------------------------------------------------
// Component tracking
// -----------------------------------------------------------------------

bool DQPlotController::isEmptyAfterReset()
{
    auto system = Application::getInstance().getSystem();
    std::lock_guard<std::mutex> lock(resetMutex);

    for (auto it = outcomes.begin(); it != outcomes.end();) {
        if (!system->hasOutcome(it->first)) { auto name = it->first; ++it; removeComponent(name); }
        else ++it;
    }
    for (auto it = probes.begin(); it != probes.end();) {
        if (!system->hasProbe(it->first)) { auto name = it->first; ++it; removeComponent(name); }
        else ++it;
    }
    for (auto it = operators.begin(); it != operators.end();) {
        if (!system->hasOperator(it->first)) { auto name = it->first; ++it; removeComponent(name); }
        else ++it;
    }
    return outcomes.empty() && probes.empty() && operators.empty();
}

bool DQPlotController::containsComponent(std::string name)
{
    return outcomes.count(name) || probes.count(name) || operators.count(name);
}

std::vector<std::string> DQPlotController::getComponents()
{
    std::vector<std::string> v;
    v.reserve(outcomes.size() + probes.size() + operators.size());
    for (const auto &kv : probes)    v.push_back(kv.first);
    for (const auto &kv : outcomes)  v.push_back(kv.first);
    for (const auto &kv : operators) v.push_back(kv.first);
    return v;
}

void DQPlotController::setTitle()
{
    std::string title = "Plot of: ";
    for (const auto &name : getComponents()) title += name + " ";
    plot->setTitle(QString::fromStdString(title));
}

void DQPlotController::editPlot(const std::vector<std::string> &selectedItems)
{
    for (const auto &name : getComponents())
        if (std::find(selectedItems.begin(), selectedItems.end(), name) == selectedItems.end())
            removeComponent(name);

    auto system = Application::getInstance().getSystem();
    for (const auto &name : selectedItems)
        if (!containsComponent(name))
            addComponent(name, system->hasOutcome(name));
    setTitle();
}

void DQPlotController::addComponent(const std::string &name, bool isOutcome)
{
    auto system = Application::getInstance().getSystem();
    if (isOutcome) addOutcomeSeries(name);
    else           addExpressionSeries(name, system->hasProbe(name));
    setTitle();
}

void DQPlotController::removeComponent(const std::string &name)
{
    if      (outcomes.count(name))  removeOutcomeSeries(name);
    else if (probes.count(name))    removeExpressionSeries(name, true);
    else if (operators.count(name)) removeExpressionSeries(name, false);
    else return;
    setTitle();
}

// -----------------------------------------------------------------------
// Series factory helpers
// -----------------------------------------------------------------------

QLineSeries *DQPlotController::createAndAddLineSeries(const std::string &name)
{
    auto *s = new QLineSeries();
    plot->addSeries(s, name);
    return s;
}

QAreaSeries *DQPlotController::createAndAddAreaSeries(const std::string &name, const QColor &color)
{
    auto *upper = new QLineSeries();
    auto *lower = new QLineSeries();
    auto *area  = new QAreaSeries(upper, lower);
    plot->addAreaSeries(area, name, color);
    return area;
}

// -----------------------------------------------------------------------
// Add / remove series
// -----------------------------------------------------------------------

void DQPlotController::addOutcomeSeries(const std::string &name)
{
    auto system = Application::getInstance().getSystem();
    QColor base = ColorRegistry::getColorFor(name);

    auto *outcomeS    = createAndAddLineSeries(name + "(O) observed");
    auto *boundsArea  = createAndAddAreaSeries(name + "(O) bounds", base);
    auto *upperBoundS = createAndAddLineSeries(name + "(O) upper bound");
    auto *lowerBoundS = createAndAddLineSeries(name + "(O) lower bound");
    auto *meanS       = createAndAddLineSeries(name + "(O) mean");
    auto *qtaS        = createAndAddLineSeries(name + "(O) qta");

    OutcomeSeries s = {outcomeS, boundsArea, upperBoundS, lowerBoundS, meanS, qtaS};
    applyBoundsModeToOutcome(s);
    outcomes[name] = {s, system->getOutcome(name)};
}

void DQPlotController::addExpressionSeries(const std::string &name, bool isProbe)
{
    auto system = Application::getInstance().getSystem();
    QColor base = ColorRegistry::getColorFor(name);

    auto *obsS            = createAndAddLineSeries(name + "(O) observed");
    auto *obsBoundsArea   = createAndAddAreaSeries(name + "(O) bounds", base);
    auto *obsUpperBoundS  = createAndAddLineSeries(name + "(O) upper bound");
    auto *obsLowerBoundS  = createAndAddLineSeries(name + "(O) lower bound");
    auto *obsMeanS        = createAndAddLineSeries(name + "(O) mean");
    auto *calcS           = createAndAddLineSeries(name + "(C) calculated");
    auto *calcBoundsArea  = createAndAddAreaSeries(name + "(C) bounds", base);
    auto *calcUpperBoundS = createAndAddLineSeries(name + "(C) upper bound");
    auto *calcLowerBoundS = createAndAddLineSeries(name + "(C) lower bound");
    auto *calcMeanS       = createAndAddLineSeries(name + "(C) mean");
    auto *qtaS            = createAndAddLineSeries(name + " qta");

    ExpressionSeries s = {obsS, obsBoundsArea, obsUpperBoundS, obsLowerBoundS, obsMeanS,
                          calcS, calcBoundsArea, calcUpperBoundS, calcLowerBoundS, calcMeanS, qtaS};
    applyBoundsModeToExpression(s);
    if (isProbe) probes[name]    = {s, system->getProbe(name)};
    else         operators[name] = {s, system->getOperator(name)};
}

void DQPlotController::removeOutcomeSeries(const std::string &name)
{
    auto &s = outcomes[name].first;
    plot->removeSeries(s.outcomeS);   delete s.outcomeS;
    plot->removeSeries(s.boundsArea); delete s.boundsArea;
    plot->removeSeries(s.upperBoundS); delete s.upperBoundS;
    plot->removeSeries(s.lowerBoundS); delete s.lowerBoundS;
    plot->removeSeries(s.meanS);      delete s.meanS;
    plot->removeSeries(s.qtaS);       delete s.qtaS;
    outcomes.erase(name);
}

void DQPlotController::removeExpressionSeries(const std::string &name, bool isProbe)
{
    auto &s = isProbe ? probes[name].first : operators[name].first;
    plot->removeSeries(s.obsS);            delete s.obsS;
    plot->removeSeries(s.obsBoundsArea);   delete s.obsBoundsArea;
    plot->removeSeries(s.obsUpperBoundS);  delete s.obsUpperBoundS;
    plot->removeSeries(s.obsLowerBoundS);  delete s.obsLowerBoundS;
    plot->removeSeries(s.obsMeanS);        delete s.obsMeanS;
    plot->removeSeries(s.calcS);           delete s.calcS;
    plot->removeSeries(s.calcBoundsArea);  delete s.calcBoundsArea;
    plot->removeSeries(s.calcUpperBoundS); delete s.calcUpperBoundS;
    plot->removeSeries(s.calcLowerBoundS); delete s.calcLowerBoundS;
    plot->removeSeries(s.calcMeanS);       delete s.calcMeanS;
    plot->removeSeries(s.qtaS);            delete s.qtaS;
    if (isProbe) probes.erase(name);
    else         operators.erase(name);
}

// -----------------------------------------------------------------------
// Update — runs on the GUI thread
// -----------------------------------------------------------------------

void DQPlotController::update(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    std::lock_guard<std::mutex> lock(updateMutex);
    double xMax = 0;

    for (auto &[name, p] : outcomes)
        xMax = std::max(xMax, updateOutcome(p.first, p.second, timeLowerBound, timeUpperBound));
    for (auto &[name, p] : probes)
        if (p.second) xMax = std::max(xMax, updateProbe(p.first, p.second, timeLowerBound, timeUpperBound));
    for (auto &[name, p] : operators)
        if (p.second) xMax = std::max(xMax, updateOperator(p.first, p.second, timeLowerBound, timeUpperBound));

    plot->updateXRange(xMax);
}

// -----------------------------------------------------------------------
// Data helpers
// -----------------------------------------------------------------------

static QVector<QPointF> buildCdfPoints(const DeltaQ &dq, const std::vector<Bound> &bounds,
                                        QVector<QPointF> &upper, QVector<QPointF> &lower,
                                        QVector<QPointF> &mean)
{
    int    n        = dq.getBins();
    double binWidth = dq.getBinWidth();

    QVector<QPointF> cdf;
    cdf.reserve(n + 1);
    upper.reserve(n + 1);
    lower.reserve(n + 1);
    mean.reserve(n + 1);

    cdf.emplace_back(0.0, 0.0);
    upper.emplace_back(0.0, 0.0);
    lower.emplace_back(0.0, 0.0);
    mean.emplace_back(0.0, 0.0);

    for (int i = 0; i < n; ++i) {
        double x = binWidth * (i + 1);
        cdf.emplace_back(x, dq.cdfAt(i));
        upper.emplace_back(x, bounds[i].upperBound);
        lower.emplace_back(x, bounds[i].lowerBound);
        mean.emplace_back(x, bounds[i].mean);
    }
    return cdf;
}

static QVector<QPointF> buildQtaPoints(const QTA &qta, double maxDelay)
{
    if (!qta.defined) return {};
    QVector<QPointF> v;
    v.reserve(8);
    v.emplace_back(qta.perc_25, 0.0);
    v.emplace_back(qta.perc_25, 0.25);
    v.emplace_back(qta.perc_50, 0.25);
    v.emplace_back(qta.perc_50, 0.5);
    v.emplace_back(qta.perc_75, 0.5);
    v.emplace_back(qta.perc_75, 0.75);
    v.emplace_back(maxDelay,    0.75);
    v.emplace_back(maxDelay,    qta.cdfMax);
    return v;
}

double DQPlotController::updateOutcome(OutcomeSeries &s, const std::shared_ptr<Outcome> &outcome,
                                        uint64_t lo, uint64_t hi)
{
    double maxDelay = outcome->getMaxDelay();
    DeltaQRepr repr = outcome->getObservedDeltaQRepr(lo, hi);

    QVector<QPointF> upper, lower, mean;
    QVector<QPointF> cdf = buildCdfPoints(repr.deltaQ, repr.bounds, upper, lower, mean);
    QVector<QPointF> qta = buildQtaPoints(outcome->getQTA(), maxDelay);

    plot->setUpdatesEnabled(false);
    plot->updateSeries(s.outcomeS, cdf);
    plot->updateAreaSeries(s.boundsArea, upper, lower);
    plot->updateSeries(s.upperBoundS, upper);
    plot->updateSeries(s.lowerBoundS, lower);
    plot->updateSeries(s.meanS, mean);
    plot->updateSeries(s.qtaS,  qta);
    plot->setUpdatesEnabled(true);

    return maxDelay;
}

double DQPlotController::updateProbe(ExpressionSeries &s, std::shared_ptr<Probe> &probe,
                                      uint64_t lo, uint64_t hi)
{
    updateExpression(s,
        probe->getObservedDeltaQRepr(lo, hi),
        probe->getCalculatedDeltaQRepr(lo, hi),
        probe->getQTA(),
        probe->getMaxDelay());
    return probe->getMaxDelay();
}

double DQPlotController::updateOperator(ExpressionSeries &s, std::shared_ptr<Operator> &op,
                                         uint64_t lo, uint64_t hi)
{
    updateExpression(s,
        op->getObservedDeltaQRepr(lo, hi),
        op->getCalculatedDeltaQRepr(lo, hi),
        op->getQTA(),
        op->getMaxDelay());
    return op->getMaxDelay();
}

void DQPlotController::updateExpression(ExpressionSeries &s,
                                         DeltaQRepr &&obs, DeltaQRepr &&calc,
                                         QTA &&qta, double maxDelay)
{
    QVector<QPointF> obsUpper, obsLower, obsMean;
    QVector<QPointF> obsCdf = buildCdfPoints(obs.deltaQ, obs.bounds, obsUpper, obsLower, obsMean);

    QVector<QPointF> calcUpper, calcLower, calcMean;
    QVector<QPointF> calcCdf = buildCdfPoints(calc.deltaQ, calc.bounds, calcUpper, calcLower, calcMean);

    QVector<QPointF> qtaPoints = buildQtaPoints(qta, maxDelay);

    plot->setUpdatesEnabled(false);
    plot->updateSeries(s.obsS, obsCdf);
    plot->updateAreaSeries(s.obsBoundsArea, obsUpper, obsLower);
    plot->updateSeries(s.obsUpperBoundS, obsUpper);
    plot->updateSeries(s.obsLowerBoundS, obsLower);
    plot->updateSeries(s.obsMeanS, obsMean);
    plot->updateSeries(s.calcS, calcCdf);
    plot->updateAreaSeries(s.calcBoundsArea, calcUpper, calcLower);
    plot->updateSeries(s.calcUpperBoundS, calcUpper);
    plot->updateSeries(s.calcLowerBoundS, calcLower);
    plot->updateSeries(s.calcMeanS, calcMean);
    plot->updateSeries(s.qtaS, qtaPoints);
    plot->setUpdatesEnabled(true);
}
