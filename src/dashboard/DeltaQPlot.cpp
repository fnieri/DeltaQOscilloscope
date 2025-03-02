
#include "DeltaQPlot.h"
#include "DQPlotList.h"
#include <QChartView>
#include <QDebug>
#include <QLineSeries>

DeltaQPlot::DeltaQPlot(std::shared_ptr<System> system, const std::vector<std::string> &selectedItems, QWidget *parent)
    : QChartView(parent)
    , chart(new QChart())
    , system(system)
{
    setChart(chart);
    chart->legend()->setVisible(true);

    // Initialize axes
    axisX = new QValueAxis();
    axisY = new QValueAxis();
    axisY->setRange(0, 1.0);
    chart->addAxis(axisX, Qt::AlignBottom);
    chart->addAxis(axisY, Qt::AlignLeft);

    controller = new DQPlotController(system, this, selectedItems);
    plotList = new DQPlotList(controller, system, parent);
}

DeltaQPlot::~DeltaQPlot()
{
    delete controller;
    delete plotList;
}

void DeltaQPlot::addSeries(QLineSeries *series, std::string &name)
{
    chart->addSeries(series);
    series->setName(QString::fromStdString(name));
    series->attachAxis(axisX);
    series->attachAxis(axisY);
}

void DeltaQPlot::update()
{
    controller->update();
}

void DeltaQPlot::removeSeries(QAbstractSeries *series)
{
    chart->removeSeries(series);
}

void DeltaQPlot::editPlot(const std::vector<std::string> &selectedItems)
{
    controller->editPlot(selectedItems);
}

void DeltaQPlot::updateSeries(QLineSeries *series, const std::vector<std::pair<double, double>> &data, double xRange)
{

    series->clear();
    series->append(0, 0);
    for (const auto &[x, y] : data)
        series->append(x, y);
    axisX->setRange(0, xRange);
}
std::vector<std::string> DeltaQPlot::getComponents()
{
    return controller->getComponents();
}

DQPlotList *DeltaQPlot::getPlotList()
{
    return plotList;
}

void DeltaQPlot::mousePressEvent(QMouseEvent *event)
{
    emit plotSelected(this); // Emit signal when clicked
}
