
#ifndef DELTAQPLOT_H
#define DELTAQPLOT_H

#pragma once
#include "DQPlotController.h"
#include "DQPlotList.h"
#include <QChartView>
#include <QLineSeries>
#include <QValueAxis>
#include <memory>
#include <vector>
class DQPlotController;
class DQPlotList;
class DeltaQPlot : public QChartView
{
    Q_OBJECT

public:
    explicit DeltaQPlot(std::shared_ptr<System> system, const std::vector<std::string> &selectedItems, QWidget *parent = nullptr);
    ~DeltaQPlot();

    void addSeries(QLineSeries *series, std::string &name);
    void update();
    void removeSeries(QAbstractSeries *series);
    void editPlot(const std::vector<std::string> &selectedItems);
    std::vector<std::string> getComponents();

    void updateSeries(QLineSeries *series, const std::vector<std::pair<double, double>> &data, double xRange);

    DQPlotList *getPlotList();
    void mousePressEvent(QMouseEvent *event);

private:
    QChart *chart;
    QValueAxis *axisX;
    QValueAxis *axisY;
    QLineSeries *operationSeries;
    std::shared_ptr<System> system;
    DQPlotController *controller;
    DQPlotList *plotList;

signals:
    void plotSelected(DeltaQPlot *plot);
};

#endif // DELTAQPLOT_H
