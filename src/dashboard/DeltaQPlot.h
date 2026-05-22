#pragma once

#include "CustomLegendPanel.h"
#include <QAreaSeries>
#include <QChartView>
#include <QLineSeries>
#include <QToolButton>
#include <QValueAxis>
#include <string>
#include <vector>

class DQPlotController;
class DQPlotList;

class DeltaQPlot : public QWidget
{
    Q_OBJECT

public:
    explicit DeltaQPlot(const std::vector<std::string> &selectedItems, QWidget *parent = nullptr);
    ~DeltaQPlot();

    void addSeries(QLineSeries *series, const std::string &name);
    void addAreaSeries(QAreaSeries *series, const std::string &name, const QColor &color);
    void removeSeries(QAbstractSeries *series);

    void update(uint64_t timeLowerBound, uint64_t timeUpperBound);
    void editPlot(const std::vector<std::string> &selectedItems);

    void updateSeries(QLineSeries *series, const QVector<QPointF> &data);
    void updateAreaSeries(QAreaSeries *series, const QVector<QPointF> &upper, const QVector<QPointF> &lower);
    void setSeriesVisible(QAbstractSeries *series, bool visible);
    void updateXRange(double xRange);

    std::vector<std::string> getComponents();
    DQPlotList *getPlotList();
    bool isEmptyAfterReset();
    void setTitle(QString &&title);

protected:
    void mousePressEvent(QMouseEvent *event) override;

Q_SIGNALS:
    void plotSelected(DeltaQPlot *plot);

private:
    QChartView      *chartView;
    QChart          *chart;
    QValueAxis      *axisX;
    QValueAxis      *axisY;
    DQPlotController *controller;
    DQPlotList       *plotList;
    CustomLegendPanel *legendPanel;
};
