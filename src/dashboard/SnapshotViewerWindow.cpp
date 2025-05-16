
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

    setSnapshots(snapshotList);
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
    if (snapshots.empty() || !snapshots.count(currentObservable))
        return;

    const auto &snapshot = snapshots.at(currentObservable);
    timeLabel->setText(QString("Time Index: %1").arg(value));

    if (snapshot.getObservedSize() == 0 || value >= static_cast<int>(snapshot.getObservedSize()))
        return;

    const auto obs = snapshot.getObservedDeltaQs()[value];
    const auto calc = snapshot.getCalculatedSize() > value ? std::optional<DeltaQRepr>(snapshot.getCalculatedDeltaQs()[value]) : std::nullopt;

    QtConcurrent::run([=]() {
        int bins = obs.deltaQ.getBins();
        double binWidth = obs.deltaQ.getBinWidth();

        QVector<QPointF> obsCdf;
        obsCdf.reserve(bins);
        QVector<QPointF> obsMean, obsLower, obsUpper;
        obsMean.reserve(bins);
        obsLower.reserve(bins);
        obsUpper.reserve(bins);
        obsCdf.emplace_back(QPointF(0, 0));
        obsMean.emplace_back(QPointF(0, 0));
        obsLower.emplace_back(QPointF(0, 0));
        obsUpper.emplace_back(QPointF(0, 0));

        for (int i = 0; i < bins; ++i) {
            double x = binWidth * (i + 1);
            obsCdf.append(QPointF(x, obs.deltaQ.cdfAt(i)));
            obsLower.append(QPointF(x, obs.bounds[i].lowerBound));
            obsUpper.append(QPointF(x, obs.bounds[i].upperBound));
            obsMean.append(QPointF(x, obs.bounds[i].mean));
        }

        QVector<QPointF> calcCdf, calcMean, calcLower, calcUpper;
        if (calc) {
            int cbins = calc->deltaQ.getBins();
            double cbinWidth = calc->deltaQ.getBinWidth();
            calcCdf.reserve(cbins);
            calcMean.reserve(cbins);
            calcLower.reserve(cbins);
            calcUpper.reserve(cbins);

            calcCdf.emplace_back(QPointF(0, 0));

            calcMean.emplace_back(QPointF(0, 0));
            calcLower.emplace_back(QPointF(0, 0));
            calcUpper.emplace_back(QPointF(0, 0));
            for (int i = 0; i < cbins; ++i) {
                double x = cbinWidth * (i + 1);
                calcCdf.append(QPointF(x, calc->deltaQ.cdfAt(i)));
                calcLower.append(QPointF(x, calc->bounds[i].lowerBound));
                calcUpper.append(QPointF(x, calc->bounds[i].upperBound));
                calcMean.append(QPointF(x, calc->bounds[i].mean));
            }
        }

        // GUI update
        QMetaObject::invokeMethod(this, [=]() {
            QChart *chart = new QChart();
            chart->setTitle(QString::fromStdString(currentObservable) + QString(" - Time Index: %1").arg(value));

            auto addSeries = [&](const QVector<QPointF> &data, const QString &name, const QColor &color) {
                QLineSeries *series = new QLineSeries();
                series->setName(name);
                series->append(data);
                series->setColor(color);
                chart->addSeries(series);
                return series;
            };

            auto *obsS = addSeries(obsCdf, "Observed", Qt::blue);
            auto *obsMeanS = addSeries(obsMean, "Obs Mean", Qt::darkGreen);
            auto *obsLowerS = addSeries(obsLower, "Obs Lower", Qt::darkCyan);
            auto *obsUpperS = addSeries(obsUpper, "Obs Upper", Qt::darkMagenta);

            QLineSeries *calcS = nullptr, *calcMeanS = nullptr, *calcLowerS = nullptr, *calcUpperS = nullptr;
            if (!calcCdf.isEmpty()) {
                calcS = addSeries(calcCdf, "Calculated", Qt::red);
                calcMeanS = addSeries(calcMean, "Calc Mean", Qt::darkYellow);
                calcLowerS = addSeries(calcLower, "Calc Lower", Qt::gray);
                calcUpperS = addSeries(calcUpper, "Calc Upper", Qt::lightGray);
            }

            auto *axisX = new QValueAxis();
            axisX->setTitleText("Delay (ms)");
            chart->addAxis(axisX, Qt::AlignBottom);

            auto *axisY = new QValueAxis();
            axisY->setTitleText("CDF");
            chart->addAxis(axisY, Qt::AlignLeft);

            for (auto *series : chart->series()) {
                series->attachAxis(axisX);
                series->attachAxis(axisY);
            }

            chartView->setChart(chart);
        });
    });
}
