#pragma once

#include <QChartView>
#include <QComboBox>
#include <QLabel>
#include <QSlider>
#include <QWidget>

#include "../maths/Snapshot.h"
#include <map>
class SnapshotViewerWindow : public QWidget
{
    Q_OBJECT

public:
    explicit SnapshotViewerWindow(QWidget *parent = nullptr);
    void setSnapshots(std::vector<Snapshot> &snapshotList);

private Q_SLOTS:
    void onObservableChanged(const QString &name);
    void onTimeSliderChanged(int value);

private:
    void updatePlot();

    QChartView *chartView;
    QComboBox *observableSelector;
    QSlider *timeSlider;
    QLabel *timeLabel;

    std::map<std::string, Snapshot> snapshots;
    std::string currentObservable;
};
