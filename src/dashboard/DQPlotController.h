
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

struct OutcomeSeries {
    QLineSeries *outcomeS;
    QLineSeries *lowerBoundS;
    QLineSeries *upperBoundS;
    QLineSeries *meanS;
    QLineSeries *qtaS;
};
;
struct ProbeAllSeries {
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
    void addComponent(const std::string &name, bool isProbe);

    QLineSeries *createAndAddLineSeries(const std::string &legendName);

    void addOutcomeSeries(const std::string &name);

    void removeOutcomeSeries(const std::string &name);

    void addProbeSeries(const std::string &name);

    void removeProbeSeries(const std::string &name);
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

    bool isEmptyAfterReset();

private:
    double updateOutcome(OutcomeSeries series, const std::shared_ptr<Outcome> &outcome, uint64_t timeLowerBound, uint64_t timeUpperBound);
    void updateProbe(ProbeAllSeries probeSeries, std::shared_ptr<Probe> &probe, uint64_t timeLowerBound, uint64_t timeUpperBound);

    DeltaQPlot *plot;

    std::mutex updateMutex;
    std::mutex resetMutex;

    std::map<std::string, std::pair<OutcomeSeries, std::shared_ptr<Outcome>>> outcomes;
    std::map<std::string, std::pair<ProbeAllSeries, std::shared_ptr<Probe>>> probes;
};

#endif // DQPLOTCONTROLLER_H
