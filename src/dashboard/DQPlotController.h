#pragma once

#include "../diagram/System.h"
#include "../maths/DeltaQ.h"
#include "AddPlotDialog.h"
#include "DeltaQPlot.h"
#include <QLineSeries>
#include <memory>

class DeltaQPlot;
class DQPlotController
{
    std::shared_ptr<System> system;
    std::unordered_map<std::string, std::pair<QLineSeries *, std::shared_ptr<Probe>>> probes;
    std::unordered_map<std::string, std::pair<QLineSeries *, std::shared_ptr<Outcome>>> outcomes;
    DeltaQ (*operation)(const std::vector<DeltaQ> &) = nullptr;

    DeltaQPlot *plot;

    void updateComponent(QLineSeries *series, std::shared_ptr<Probe> component, double binWidth);

public:
    DQPlotController(std::shared_ptr<System> system, DeltaQPlot *plot, SelectionResult selection);

    void editPlot(SelectionResult selection);

    void addComponent(std::string name, bool isProbe);

    void removeComponent(std::string &&name);

    bool containsComponent(std::string name);

    void update();

    std::vector<std::string> getComponents();

    void setOperation(DeltaQ (*newOperation)(const std::vector<DeltaQ> &));
};
