
#ifndef DQPLOTCONTROLLER_H
#define DQPLOTCONTROLLER_H

#include <qlineseries.h>
#pragma once

// Project includes
#include "../diagram/System.h"
#include "DeltaQPlot.h"

// Qt includes
#include <QString>
#include <QtCharts/QLineSeries>

// C++ includes
#include <map>
#include <memory>
#include <string>
#include <vector>

// All series pertaining to an outcome
struct OutcomeSeries {
    QLineSeries *outcomeS;
    QLineSeries *lowerBoundS;
    QLineSeries *upperBoundS;
    QLineSeries *meanS;
    QLineSeries *qtaS;
};

// All series pertaining to a probe
struct ExpressionSeries {
    QLineSeries *obsS;
    QLineSeries *obsLowerBoundS;
    QLineSeries *obsUpperBoundS;
    QLineSeries *obsMeanS;
    QLineSeries *calcS;
    QLineSeries *calcLowerBoundS;
    QLineSeries *calcUpperBoundS;
    QLineSeries *calcMeanS;
    QLineSeries *qtaS;
};

class DeltaQPlot;

/**
 * @class DQPlotController
 * @brief Controls the data management and update logic of DeltaQPlot.
 */
class DQPlotController
{
public:
    /**
     * @brief Constructor with associated plot and selected components.
     */
    DQPlotController(DeltaQPlot *plot, const std::vector<std::string> &selectedItems);

    /**
     * @brief Destructor.
     */
    ~DQPlotController();

    /**
     * @brief Checks if a component is already being plotted.
     */
    bool containsComponent(std::string name);

    /**
     * @brief Updates the plot according to a new list of selected components.
     */
    void editPlot(const std::vector<std::string> &selectedItems);

    /**
     * @brief Adds a new component (probe or outcome) to the plot.
     */
    void addComponent(const std::string &name, bool isOutcome);

    QLineSeries *createAndAddLineSeries(const std::string &legendName);

    void addOutcomeSeries(const std::string &name);

    void removeOutcomeSeries(const std::string &name);

    void addExpressionSeries(const std::string &name, bool isProbe);

    void removeExpressionSeries(const std::string &name, bool isProbe);
    /**
     * @brief Returns a list of names of all currently plotted components.
     */
    std::vector<std::string> getComponents();
    /**
     * @brief Removes a plotted component by name (const reference).
     */
    void removeComponent(const std::string &name);

    /**
     * @brief Updates the data of all series based on provided time range and bin width.
     */
    void update(uint64_t timeLowerBound, uint64_t timeUpperBound);

    void setTitle();
    bool isEmptyAfterReset();

private:
    double updateOutcome(OutcomeSeries &, const std::shared_ptr<Outcome> &, uint64_t, uint64_t);

    double updateProbe(ExpressionSeries &, std::shared_ptr<Probe> &, uint64_t, uint64_t);

    double updateOperator(ExpressionSeries &, std::shared_ptr<Operator> &, uint64_t, uint64_t);

    void updateExpression(ExpressionSeries &, DeltaQRepr &&, DeltaQRepr &&, QTA &&, double maxDelay);

    DeltaQPlot *plot;

    std::mutex updateMutex;
    std::mutex resetMutex;

    std::map<std::string, std::pair<OutcomeSeries, std::shared_ptr<Outcome>>> outcomes;
    std::map<std::string, std::pair<ExpressionSeries, std::shared_ptr<Probe>>> probes;
    std::map<std::string, std::pair<ExpressionSeries, std::shared_ptr<Operator>>> operators;
};

#endif // DQPLOTCONTROLLER_H
