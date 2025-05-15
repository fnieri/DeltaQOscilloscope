
#include "SnapshotViewerWindow.h"

#include <QComboBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QSlider>
#include <QVBoxLayout>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>

SnapshotViewerWindow::SnapshotViewerWindow(QWidget *parent)
    : QWidget(parent)
    , observableSelector(new QComboBox(this))
    , timeSlider(new QSlider(Qt::Horizontal, this))
    , timeLabel(new QLabel(this))
    , chartView(new QChartView(new QChart(), this))
{

    auto *mainLayout = new QHBoxLayout(this);

    // Chart area
    chartView->setRenderHint(QPainter::Antialiasing);
    mainLayout->addWidget(chartView, 3); // 3/4 of space

    // Controls
    auto *controlLayout = new QVBoxLayout();
    controlLayout->addWidget(new QLabel("Select Observable:"));
    controlLayout->addWidget(observableSelector);

    controlLayout->addWidget(new QLabel("Select Time:"));
    controlLayout->addWidget(timeSlider);
    controlLayout->addWidget(timeLabel);

    controlLayout->addStretch();
    mainLayout->addLayout(controlLayout, 1); // 1/4 of space

    connect(observableSelector, &QComboBox::currentTextChanged, this, &SnapshotViewerWindow::onObservableChanged);
    connect(timeSlider, &QSlider::valueChanged, this, &SnapshotViewerWindow::onTimeSliderChanged);
}

void SnapshotViewerWindow::setSnapshots(std::vector<Snapshot> &snapshotList)
{
    snapshots.clear();
    observableSelector->clear();

    for (auto &s : snapshotList) {
        snapshots[s.getName()] = s;
        observableSelector->addItem(QString::fromStdString(s.getName()));
    }

    if (!snapshots.empty()) {
        observableSelector->setCurrentIndex(0);
        onObservableChanged(observableSelector->currentText());
    }
}

void SnapshotViewerWindow::onObservableChanged(const QString &name)
{
    currentObservable = name.toStdString();

    const auto &snapshot = snapshots.at(currentObservable);
    int count = static_cast<int>(snapshot.getObservedSize());

    timeSlider->setRange(0, std::max(0, count - 1));
    timeSlider->setValue(0);
    onTimeSliderChanged(0);
}

void SnapshotViewerWindow::onTimeSliderChanged(int value)
{
    const auto &snapshot = snapshots.at(currentObservable);
    timeLabel->setText(QString("Time Index: %1").arg(value));

    if (snapshot.getObservedSize() == 0)
        return;
    if (value >= static_cast<int>(snapshot.getObservedSize()))
        return;

    const auto &obs = snapshot.getObservedDeltaQs()[value];

    QChart *chart = new QChart();
    chart->setTitle(QString::fromStdString(currentObservable + " - Time Index: " + std::to_string(value)));

    auto *obsSeries = new QLineSeries();
    obsSeries->setName("Observed");
    const auto &obsVals = obs.deltaQ.getPdfValues();
    for (int i = 0; i < static_cast<int>(obsVals.size()); ++i) {
        obsSeries->append(i * obs.deltaQ.getBinWidth(), obsVals[i]);
    }
    chart->addSeries(obsSeries);

    // Calculated series
    if (snapshot.getCalculatedSize() > value) {
        const auto &calc = snapshot.getCalculatedDeltaQs()[value];
        auto *calcSeries = new QLineSeries();
        calcSeries->setName("Calculated");
        const auto &calcVals = calc.deltaQ.getPdfValues();
        for (int i = 0; i < static_cast<int>(calcVals.size()); ++i) {
            calcSeries->append(i * calc.deltaQ.getBinWidth(), calcVals[i]);
        }
        chart->addSeries(calcSeries);
    }

    auto *axisX = new QValueAxis();
    axisX->setTitleText("x");
    chart->addAxis(axisX, Qt::AlignBottom);
    obsSeries->attachAxis(axisX);

    auto *axisY = new QValueAxis();
    axisY->setTitleText("PDF");
    chart->addAxis(axisY, Qt::AlignLeft);
    obsSeries->attachAxis(axisY);

    if (snapshot.getCalculatedSize() > value) {
        chart->series().last()->attachAxis(axisX);
        chart->series().last()->attachAxis(axisY);
    }

    chartView->setChart(chart);
}
