
#ifndef DQPLOTCONTROLLER_H
#define DQPLOTCONTROLLER_H
#pragma once
#include "../diagram/System.h"
#include "DeltaQPlot.h"
#include <QString>
#include <QtCharts/QLineSeries>
#include <map>
#include <memory>
#include <vector>
class DeltaQPlot;
class DQPlotController
{
public:
    DQPlotController(DeltaQPlot *plot, const std::vector<std::string> &selectedItems);
    ~DQPlotController();
    bool containsComponent(std::string name);
    void editPlot(const std::vector<std::string> &selectedItems);
    void addComponent(std::string name, bool isProbe);
    std::vector<std::string> getComponents();
    void removeComponent(std::string &&name);
    void removeComponent(const std::string &name);
    void update(double binWidth);

private:
    void updateOutcome(QLineSeries *series, std::shared_ptr<Outcome> outcome, double binWidth);
    void updateProbe(QLineSeries *probeSeries, QLineSeries *calculatedProbeSeries, std::shared_ptr<Probe> probe, double binWidth);

    DeltaQPlot *plot;

    std::map<std::string, std::pair<QLineSeries *, std::shared_ptr<Outcome>>> outcomes;
    std::map<std::string, std::tuple<QLineSeries *, QLineSeries *, std::shared_ptr<Probe>>> probes;
};

#endif // DQPLOTCONTROLLER_H
