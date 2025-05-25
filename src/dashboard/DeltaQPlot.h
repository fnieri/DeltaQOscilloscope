
#ifndef DELTAQPLOT_H
#define DELTAQPLOT_H

#include "CustomLegendPanel.h"
#include <qboxlayout.h>
#pragma once

// Qt includes
#include <QChartView>
#include <QLineSeries>
#include <QToolButton>
#include <QValueAxis>
// C++ includes
#include <string>
#include <vector>

class DQPlotController;
class DQPlotList;

/**
 * @class DeltaQPlot
 * @brief A class representing a DeltaQ chart view that allows visualization of probes over time.
 */
class DeltaQPlot : public QWidget
{
    Q_OBJECT

public:
    /**
     * @brief Constructs a DeltaQPlot with selected components.
     * @param selectedItems List of selected component names.
     * @param parent Parent widget.
     */
    explicit DeltaQPlot(const std::vector<std::string> &selectedItems, QWidget *parent = nullptr);

    /**
     * @brief Destructor.
     */
    ~DeltaQPlot();

    /**
     * @brief Adds a QLineSeries to the chart.
     * @param series Pointer to the line series.
     * @param name Name of the series.
     */
    void addSeries(QLineSeries *series, const std::string &name);

    /**
     * @brief Updates the plot data using provided time range and bin width.
     */
    void update(uint64_t timeLowerBound, uint64_t timeUpperBound);

    /**
     * @brief Removes a series from the chart.
     * @param series Series to remove.
     */
    void removeSeries(QAbstractSeries *series);

    /**
     * @brief Updates plot with new set of selected components.
     */
    void editPlot(const std::vector<std::string> &selectedItems);

    /**
     * @brief Gets list of currently plotted component names.
     */
    std::vector<std::string> getComponents();

    bool isEmptyAfterReset();

    /**
     * @brief Updates an existing series with new data points.
     */
    void updateSeries(QLineSeries *series, const QVector<QPointF> &data);

    void updateXRange(double xRange);
    /**
     * @brief Returns the associated plot list.
     */
    DQPlotList *getPlotList();

    void setTitle(QString &&);

protected:
    /**
     * @brief Handles mouse press events on the chart.
     */
    void mousePressEvent(QMouseEvent *event) override;

Q_SIGNALS:
    /**
     * @brief Emitted whenout
    QHBox this plot is selected by the user.
     */
    void plotSelected(DeltaQPlot *plot);

private:
    QHBoxLayout *layout;
    QToolButton *toggleButton;
    QChartView *chartView;
    QChart *chart;

    QValueAxis *axisX;
    QValueAxis *axisY;

    QLineSeries *operationSeries;
    DQPlotController *controller;
    DQPlotList *plotList;

    CustomLegendPanel *legendPanel;
};

#endif // DELTAQPLOT_H
