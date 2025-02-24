
#ifndef OUTCOME_PLOT_H
#define OUTCOME_PLOT_H
#pragma once

#include "../diagram/System.h"
#include "AddPlotDialog.h"
#include "DQPlotController.h"
#include <QChartView>
#include <QLineSeries>
#include <QValueAxis>
#include <memory>
#include <qlineseries.h>

class DQPlotController;
class DeltaQPlot : public QChartView
{
    Q_OBJECT

public:
    explicit DeltaQPlot(std::shared_ptr<System> system, SelectionResult selection, QWidget *parent = nullptr);
    ~DeltaQPlot();
    void update();
    void updateSeries(QLineSeries *series, const std::vector<std::pair<double, double>> &data, double xRange);

    void updateOperationSeries(const std::vector<std::pair<double, double>> &data, double xRange);
    void removeSeries(QAbstractSeries *series);
    void updateOperation();
    void editPlot(SelectionResult selection);

    std::vector<std::string> getComponents();
    void addSeries(QLineSeries *series, std::string &name);

private:
    DQPlotController *controller;

    std::shared_ptr<System> system;

    QLineSeries *operationSeries;
    QChart *chart;
    QValueAxis *axisX;
    QValueAxis *axisY;
};

#endif // OUTCOME_PLOT_H
