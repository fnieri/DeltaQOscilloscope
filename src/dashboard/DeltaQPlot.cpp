
#include "DeltaQPlot.h"
#include "DQPlotList.h"
#include <QChartView>
#include <QDebug>
#include <QLineSeries>

DeltaQPlot::DeltaQPlot(const std::vector<std::string> &selectedItems, QWidget *parent)
    : QChartView(parent)
    , chart(new QChart())
{
    setChart(chart);
    chart->legend()->setVisible(true);

    // Initialize axes
    axisX = new QValueAxis();
    axisY = new QValueAxis();
    axisY->setRange(-0.1, 1.0);
    chart->addAxis(axisX, Qt::AlignBottom);
    chart->addAxis(axisY, Qt::AlignLeft);
    axisX->setRange(0, 0.05);
    controller = new DQPlotController(this, selectedItems);
    plotList = new DQPlotList(controller, this);
}

DeltaQPlot::~DeltaQPlot()
{
    delete controller;
    controller = NULL;
    delete plotList;
    plotList = NULL;
    delete chart;
    chart = NULL;
}

bool DeltaQPlot::isEmptyAfterReset()
{
    if (!controller->isEmptyAfterReset()) {
        plotList->updateLists();
        return false;
    }
    return true;
}

void DeltaQPlot::addSeries(QLineSeries *series, const std::string &name)
{
    chart->addSeries(series);
    series->setName(QString::fromStdString(name));
    series->attachAxis(axisX);
    series->attachAxis(axisY);
}

void DeltaQPlot::update(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    controller->update(timeLowerBound, timeUpperBound);
}

void DeltaQPlot::removeSeries(QAbstractSeries *series)
{
    chart->removeSeries(series);
}

void DeltaQPlot::editPlot(const std::vector<std::string> &selectedItems)
{
    controller->editPlot(selectedItems);
}

void DeltaQPlot::updateSeries(QLineSeries *series, const std::vector<std::pair<double, double>> &data)
{

    QVector<QPointF> points;
    points.prepend(QPointF(0, 0));
    points.reserve(data.size());
    for (const auto &[x, y] : data)
        points.append(QPointF(x, y));
    series->replace(points);
}

void DeltaQPlot::updateXRange(double xRange)
{
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
    Q_EMIT plotSelected(this);
}
