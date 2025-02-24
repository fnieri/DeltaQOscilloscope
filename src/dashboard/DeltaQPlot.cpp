
#include "DeltaQPlot.h"
#include <QChartView>
#include <QDebug>
#include <QLineSeries>

DeltaQPlot::DeltaQPlot(std::shared_ptr<System> system, SelectionResult selection, QWidget *parent)
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

    // Initialize the operation series
    operationSeries = new QLineSeries();
    chart->addSeries(operationSeries);
    operationSeries->attachAxis(axisX);
    operationSeries->attachAxis(axisY);

    controller = new DQPlotController(system, this, selection);
}

DeltaQPlot::~DeltaQPlot()
{
    delete controller; // Use delete instead of free()
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

void DeltaQPlot::editPlot(SelectionResult selection)
{
    controller->editPlot(selection);
}

void DeltaQPlot::updateSeries(QLineSeries *series, const std::vector<std::pair<double, double>> &data, double xRange)
{
    series->clear();
    for (const auto &[x, y] : data)
        series->append(x, y);
    axisX->setRange(0, xRange);
}

void DeltaQPlot::updateOperationSeries(const std::vector<std::pair<double, double>> &data, double xRange)
{
    operationSeries->clear();
    for (const auto &[x, y] : data)
        operationSeries->append(x, y);

    axisX->setRange(0, xRange);
}

std::vector<std::string> DeltaQPlot::getComponents()
{

    return controller->getComponents();
}
