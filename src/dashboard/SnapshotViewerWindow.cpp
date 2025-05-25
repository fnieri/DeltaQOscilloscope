/**
 * @file SnapshotViewerWindow.cpp
 * @brief Implementation of the SnapshotViewerWindow class.
 */

#include "SnapshotViewerWindow.h"

#include <QComboBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QSlider>
#include <QVBoxLayout>
#include <QtCharts/QChartView>
#include <QtCharts/QLineSeries>
#include <QtCharts/QValueAxis>
#include <QtConcurrent>
#include <qnamespace.h>

/**
 * @brief Constructs a new SnapshotViewerWindow.
 */
SnapshotViewerWindow::SnapshotViewerWindow(std::vector<Snapshot> &snapshotList, QWidget *parent)
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
    auto *controlLayout = new QVBoxLayout(this);
    controlLayout->addWidget(new QLabel("Select Observable:"));
    controlLayout->addWidget(observableSelector);

    controlLayout->addWidget(new QLabel("Select Time:"));
    controlLayout->addWidget(timeSlider);
    controlLayout->addWidget(timeLabel);
    controlLayout->addStretch();
    mainLayout->addLayout(controlLayout, 1); // 1/4 of space

    connect(observableSelector, &QComboBox::currentTextChanged, this, &SnapshotViewerWindow::onObservableChanged);
    connect(timeSlider, &QSlider::valueChanged, this, &SnapshotViewerWindow::onTimeSliderChanged);

    setSnapshots(snapshotList);
}

/**
 * @brief Sets the snapshot data and updates the UI.
 */
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

/**
 * @brief Handles the change of observable by updating the slider range and triggering a plot update.
 */
void SnapshotViewerWindow::onObservableChanged(const QString &name)
{
    currentObservable = name.toStdString();

    const auto &snapshot = snapshots.at(currentObservable);
    int count = static_cast<int>(snapshot.getObservedSize());
    timeSlider->setRange(0, std::max(0, count - 1));
    timeSlider->setValue(0);
    onTimeSliderChanged(0);
}

/**
 * @brief Handles changes in the time slider by updating the chart.
 */
void SnapshotViewerWindow::onTimeSliderChanged(int value)
{
    if (snapshots.empty() || !snapshots.count(currentObservable))
        return;

    const auto &snapshot = snapshots.at(currentObservable);

    if (snapshot.getObservedSize() == 0 || value >= static_cast<int>(snapshot.getObservedSize()))
        return;

    const auto obs = snapshot.getObservedDeltaQs()[value];
    const auto calc = snapshot.getCalculatedSize() > value ? std::optional<DeltaQRepr>(snapshot.getCalculatedDeltaQs()[value]) : std::nullopt;
    const auto qta = snapshot.getQTAs()[value];
    auto time = obs.time;

    qint64 msTime = time / 1000000;
    QDateTime timestamp = QDateTime::fromMSecsSinceEpoch(msTime);
    timeLabel->setText(QString("Snapshot at: %1").arg(timestamp.toString()));

    auto ret = QtConcurrent::run([=]() {
        int bins = obs.deltaQ.getBins();
        double binWidth = obs.deltaQ.getBinWidth();

        QVector<QPointF> obsMean, obsLower, obsUpper, obsCdf, qtaData;
        obsCdf.reserve(bins + 1);
        obsMean.reserve(bins + 1);
        obsLower.reserve(bins + 1);
        obsUpper.reserve(bins + 1);

        obsCdf.append(QPointF(0, 0));
        obsMean.append(QPointF(0, 0));
        obsLower.append(QPointF(0, 0));
        obsUpper.append(QPointF(0, 0));

        for (int i = 0; i < bins; ++i) {
            double x = binWidth * (i + 1);
            obsCdf.append(QPointF(x, obs.deltaQ.cdfAt(i)));
            obsLower.append(QPointF(x, obs.bounds[i].lowerBound));
            obsUpper.append(QPointF(x, obs.bounds[i].upperBound));
            obsMean.append(QPointF(x, obs.bounds[i].mean));
        }

        if (qta.defined) {
            double maxDelay = bins * binWidth;
            qtaData = {
                {qta.perc_25, 0         },
                {qta.perc_25, 0.25      },
                {qta.perc_50, 0.25      },
                {qta.perc_50, 0.5       },
                {qta.perc_75, 0.5       },
                {qta.perc_75, 0.75      },
                {maxDelay,    0.75      },
                {maxDelay,    qta.cdfMax}
            };
        }

        QVector<QPointF> calcCdf, calcMean, calcLower, calcUpper;
        if (calc) {
            int cbins = calc->deltaQ.getBins();
            double cbinWidth = calc->deltaQ.getBinWidth();
            calcCdf.reserve(cbins + 1);
            calcMean.reserve(cbins + 1);
            calcLower.reserve(cbins + 1);
            calcUpper.reserve(cbins + 1);

            calcCdf.append(QPointF(0, 0));
            calcMean.append(QPointF(0, 0));
            calcLower.append(QPointF(0, 0));
            calcUpper.append(QPointF(0, 0));

            for (int i = 0; i < cbins; ++i) {
                double x = cbinWidth * (i + 1);
                calcCdf.append(QPointF(x, calc->deltaQ.cdfAt(i)));
                calcLower.append(QPointF(x, calc->bounds[i].lowerBound));
                calcUpper.append(QPointF(x, calc->bounds[i].upperBound));
                calcMean.append(QPointF(x, calc->bounds[i].mean));
            }
        }

        QMetaObject::invokeMethod(this, [=]() {
            QChart *chart = chartView->chart();

            // Clear previous content
            chart->removeAllSeries();
            const auto axes = chart->axes();
            for (QAbstractAxis *axis : axes) {
                chart->removeAxis(axis);
                axis->deleteLater(); // Safe axis cleanup
            }

            chart->setTitle(QString::fromStdString(currentObservable) + QString(" - Time Index: %1").arg(value));

            auto addSeries = [&](const QVector<QPointF> &data, const QString &name, const QColor &color) {
                QLineSeries *series = new QLineSeries();
                series->setName(name);
                series->append(data);
                series->setColor(color);
                chart->addSeries(series);
                return series;
            };

            addSeries(obsCdf, "Observed", Qt::blue);
            addSeries(obsMean, "Obs Mean", Qt::yellow);
            addSeries(obsLower, "Obs Lower", Qt::green);
            addSeries(obsUpper, "Obs Upper", Qt::darkGreen);
            addSeries(qtaData, "QTA", Qt::darkBlue);

            if (!calcCdf.isEmpty()) {
                addSeries(calcCdf, "Calculated", Qt::red);
                addSeries(calcMean, "Calc Mean", Qt::darkYellow);
                addSeries(calcLower, "Calc Lower", Qt::magenta);
                addSeries(calcUpper, "Calc Upper", Qt::darkMagenta);
            }

            auto *axisX = new QValueAxis();
            axisX->setTitleText("Delay (s)");
            chart->addAxis(axisX, Qt::AlignBottom);

            auto *axisY = new QValueAxis();
            axisY->setTitleText("ΔQ(x)");
            axisY->setRange(0, 1);
            chart->addAxis(axisY, Qt::AlignLeft);

            for (auto *series : chart->series()) {
                series->attachAxis(axisX);
                series->attachAxis(axisY);
            }
        });
    });
}
