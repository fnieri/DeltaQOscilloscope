#include "Dashboard.h"
#include "DeltaQHelpers.h"
#include "src/maths/DeltaQ.h"
#include <QApplication>
#include <QLineSeries>
#include <QMainWindow>
#include <QtCharts/QChart>
#include <QtCharts/QChartView>

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
