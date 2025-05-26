#pragma once

#include <QChartView>
#include <QComboBox>
#include <QLabel>
#include <QSlider>
#include <QWidget>

#include "../maths/Snapshot.h"
#include <map>

/**
 * @brief A QWidget-based window for visualizing snapshots from fired triggers.
 */
class SnapshotViewerWindow : public QWidget
{
    Q_OBJECT

public:
    explicit SnapshotViewerWindow(std::vector<Snapshot> &snapshotList, QWidget *parent = nullptr);

    /**
     * @brief Sets the snapshots to display in the viewer.
     * @param snapshotList A vector of Snapshots.
     */
    void setSnapshots(std::vector<Snapshot> &snapshotList);

private Q_SLOTS:
    /**
     * @brief Slot triggered when the observable selection changes.
     * @param name Name of the newly selected observable.
     */
    void onObservableChanged(const QString &name);

    /**
     * @brief Slot triggered when the time slider is moved.
     * @param value Index of the snapshot in the selected observable.
     */
    void onTimeSliderChanged(int value);

private:
    /**
     * @brief Updates the chart view based on the current observable and time index.
     */
    void updatePlot();

    QChartView *chartView;                    ///< Chart view for plotting the snapshot data.
    QComboBox *observableSelector;            ///< Dropdown for selecting an observable.
    QSlider *timeSlider;                      ///< Slider for selecting time index.
    QLabel *timeLabel;                        ///< Label showing the current time.

    std::map<std::string, Snapshot> snapshots;///< Map of observable name to snapshot.
    std::string currentObservable;            ///< Currently selected observable.
};
