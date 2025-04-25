
#ifndef DELTAQPLOT_H
#define DELTAQPLOT_H

#pragma once

// Qt includes
#include <QChartView>
#include <QLineSeries>
#include <QValueAxis>

// C++ includes
#include <string>
#include <vector>

class DQPlotController;
class DQPlotList;

/**
 * @class DeltaQPlot
 * @brief A class representing a Delta-Q chart view that allows visualization of outcomes and probes over time.
 */
class DeltaQPlot : public QChartView
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
    void addSeries(QLineSeries *series, std::string &name);

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

    /**
     * @brief Updates an existing series with new data points.
     */
    void updateSeries(QLineSeries *series, const std::vector<std::pair<double, double>> &data);

    void updateXRange(double xRange);
    /**
     * @brief Returns the associated plot list.
     */
    DQPlotList *getPlotList();

protected:
    /**
     * @brief Handles mouse press events on the chart.
     */
    void mousePressEvent(QMouseEvent *event) override;

Q_SIGNALS:
    /**
     * @brief Emitted when this plot is selected by the user.
     */
    void plotSelected(DeltaQPlot *plot);

private:
    QChart *chart;
    QValueAxis *axisX;
    QValueAxis *axisY;
    QLineSeries *operationSeries;
    DQPlotController *controller;
    DQPlotList *plotList;
};

#endif // DELTAQPLOT_H
