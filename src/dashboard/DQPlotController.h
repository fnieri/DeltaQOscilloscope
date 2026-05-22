#pragma once

#include "../diagram/System.h"
#include "DeltaQPlot.h"
#include <QAreaSeries>
#include <QLineSeries>
#include <map>
#include <memory>
#include <mutex>
#include <string>
#include <vector>

struct OutcomeSeries {
    QLineSeries *outcomeS;
    // Bounds — two representations, only one visible at a time
    QAreaSeries *boundsArea;   // area mode: filled band
    QLineSeries *upperBoundS;  // line mode: individual lines
    QLineSeries *lowerBoundS;
    QLineSeries *meanS;
    QLineSeries *qtaS;
};

struct ExpressionSeries {
    QLineSeries *obsS;
    QAreaSeries *obsBoundsArea;
    QLineSeries *obsUpperBoundS;
    QLineSeries *obsLowerBoundS;
    QLineSeries *obsMeanS;
    QLineSeries *calcS;
    QAreaSeries *calcBoundsArea;
    QLineSeries *calcUpperBoundS;
    QLineSeries *calcLowerBoundS;
    QLineSeries *calcMeanS;
    QLineSeries *qtaS;
};

class DeltaQPlot;

class DQPlotController
{
public:
    DQPlotController(DeltaQPlot *plot, const std::vector<std::string> &selectedItems);
    ~DQPlotController();

    bool containsComponent(std::string name);
    void editPlot(const std::vector<std::string> &selectedItems);
    void addComponent(const std::string &name, bool isOutcome);
    void removeComponent(const std::string &name);
    std::vector<std::string> getComponents();
    void update(uint64_t timeLowerBound, uint64_t timeUpperBound);
    void setTitle();
    bool isEmptyAfterReset();

    // Switches all bounds between filled area and individual lines.
    void setBoundsAsArea(bool useArea);
    bool getBoundsAsArea() const { return boundsAsArea; }

    QLineSeries *createAndAddLineSeries(const std::string &name);
    QAreaSeries *createAndAddAreaSeries(const std::string &name, const QColor &color);
    void addOutcomeSeries(const std::string &name);
    void removeOutcomeSeries(const std::string &name);
    void addExpressionSeries(const std::string &name, bool isProbe);
    void removeExpressionSeries(const std::string &name, bool isProbe);

private:
    double updateOutcome(OutcomeSeries &, const std::shared_ptr<Outcome> &, uint64_t, uint64_t);
    double updateProbe(ExpressionSeries &, std::shared_ptr<Probe> &, uint64_t, uint64_t);
    double updateOperator(ExpressionSeries &, std::shared_ptr<Operator> &, uint64_t, uint64_t);
    void   updateExpression(ExpressionSeries &, DeltaQRepr &&obs, DeltaQRepr &&calc, QTA &&qta, double maxDelay);

    void applyBoundsModeToOutcome(OutcomeSeries &s);
    void applyBoundsModeToExpression(ExpressionSeries &s);

    DeltaQPlot *plot;
    bool        boundsAsArea = true;
    std::mutex  updateMutex;
    std::mutex  resetMutex;

    std::map<std::string, std::pair<OutcomeSeries,    std::shared_ptr<Outcome>>>   outcomes;
    std::map<std::string, std::pair<ExpressionSeries, std::shared_ptr<Probe>>>     probes;
    std::map<std::string, std::pair<ExpressionSeries, std::shared_ptr<Operator>>>  operators;
};
