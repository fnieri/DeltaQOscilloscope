
#ifndef OUTCOME_PLOT_H
#define OUTCOME_PLOT_H
#pragma once

#include "../diagram/System.h"
#include <QChartView>
#include <QLineSeries>
#include <QValueAxis>
#include <memory>
class DeltaQPlot : public QChartView
{
    Q_OBJECT

public:
    explicit DeltaQPlot(std::shared_ptr<System> system, QWidget *parent = nullptr, DeltaQ (*operation)(const std::vector<DeltaQ> &) = nullptr);

    void addComponent(std::string name, bool isProbe);

    void removeComponent(std::string name);

    bool containsComponent(std::string name);

    void update();

    std::vector<std::string> getComponents();

    void setOperation(DeltaQ (*newOperation)(const std::vector<DeltaQ> &));

private:
    std::shared_ptr<System> system;
    std::unordered_map<std::string, std::pair<QLineSeries *, std::shared_ptr<Probe>>> probes;
    std::unordered_map<std::string, std::pair<QLineSeries *, std::shared_ptr<Outcome>>> outcomes;
    DeltaQ (*operation)(const std::vector<DeltaQ> &) = nullptr;

    QLineSeries *operationSeries;
    QChart *chart;
    QValueAxis *axisX;
    QValueAxis *axisY;
};

#endif // OUTCOME_PLOT_H
