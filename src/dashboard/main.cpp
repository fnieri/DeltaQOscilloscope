#include "DeltaQHelpers.h"
#include "src/maths/DeltaQ.h"
#include <QApplication>
#include <QLineSeries>
#include <QMainWindow>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>
/*
int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    DeltaQ deltaQ = DeltaQ(0.1);
    std::vector<double> outcomeSamples = {0.1, 0.2, 0.3, 0.4, 0.5};
    deltaQ.processSamples(outcomeSamples);

    QChart* chart = new QChart();
    chart->setTitle("DeltaQ Visualization");



    QLineSeries* cdfSeries = toQSeries(deltaQ);
    cdfSeries->setName("CDF");
    chart->addSeries(cdfSeries);

    chart->createDefaultAxes();

    QChartView* chartView = new QChartView(chart);
    chartView->setRenderHint(QPainter::Antialiasing);

    QMainWindow window;
    window.setCentralWidget(chartView);
    window.resize(800, 600);
    window.show();

    return app.exec();
}
*/

#include "Dashboard.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    // Create the Dashboard widget
    Dashboard dashboard;
    dashboard.setWindowTitle("System Editor");
    dashboard.resize(400, 300); // Set an appropriate size
    dashboard.show();

    return app.exec();
}
