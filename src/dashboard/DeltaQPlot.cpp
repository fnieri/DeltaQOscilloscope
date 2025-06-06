
#include "DeltaQPlot.h"
#include "ColorRegistry.h"
#include "CustomLegendPanel.h"
#include "DQPlotList.h"
#include <QChartView>
#include <QDebug>
#include <QHBoxLayout>
#include <QLineSeries>
#include <QMouseEvent>
#include <QRandomGenerator>
#include <QToolButton>
#include <QVBoxLayout>
DeltaQPlot::DeltaQPlot(const std::vector<std::string> &selectedItems, QWidget *parent)
    : QWidget(parent)
{

    // Create legend panel and toggle
    legendPanel = new CustomLegendPanel(this);
    QToolButton *toggleButton = new QToolButton(this);
    toggleButton->setText("> Legend");
    toggleButton->setCheckable(true);
    toggleButton->setChecked(true);
    toggleButton->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);

    connect(toggleButton, &QToolButton::toggled, legendPanel, &QWidget::setVisible);
    connect(toggleButton, &QToolButton::toggled, [toggleButton](bool checked) { toggleButton->setText(checked ? "^ Legend" : "> Legend"); });

    // Create chart and view
    chart = new QChart();
    chartView = new QChartView(chart, this);
    chartView->setRenderHint(QPainter::Antialiasing);
    chart->legend()->setVisible(false);

    axisX = new QValueAxis();
    axisY = new QValueAxis();
    axisY->setRange(-0.01, 1.0);
    axisX->setRange(0, 0.05);
    chart->addAxis(axisX, Qt::AlignBottom);
    chart->addAxis(axisY, Qt::AlignLeft);

    controller = new DQPlotController(this, selectedItems);
    plotList = new DQPlotList(controller, this);
    // Right-side layout: toggle + legend
    QVBoxLayout *rightLayout = new QVBoxLayout();
    rightLayout->addWidget(toggleButton);
    rightLayout->addWidget(legendPanel);
    rightLayout->addStretch();

    // Main layout: chart + right side
    QHBoxLayout *mainLayout = new QHBoxLayout(this);
    mainLayout->addWidget(chartView, 1);
    mainLayout->addLayout(rightLayout);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(5);
    setLayout(mainLayout);
    chartView->setRenderHint(QPainter::Antialiasing);
}

DeltaQPlot::~DeltaQPlot()
{
    delete controller;
    controller = nullptr;
    delete plotList;
    plotList = nullptr;
}

bool DeltaQPlot::isEmptyAfterReset()
{
    if (!controller->isEmptyAfterReset()) {
        plotList->updateLists();
        return false;
    }
    return true;
}

void DeltaQPlot::setTitle(QString &&title)
{
    chart->setTitle(title);
}

void DeltaQPlot::addSeries(QLineSeries *series, const std::string &name)
{
    chart->addSeries(series);
    series->setName(QString::fromStdString(name));
    series->attachAxis(axisX);
    series->attachAxis(axisY);
    QColor color = ColorRegistry::getColorFor(name);
    legendPanel->addEntry(QString::fromStdString(name), color);
    series->setColor(color);
    series->setVisible(true);
}

void DeltaQPlot::update(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    controller->update(timeLowerBound, timeUpperBound);
}

void DeltaQPlot::removeSeries(QAbstractSeries *series)
{
    auto name = series->name();
    chart->removeSeries(series);
    legendPanel->removeEntry(name);
}

void DeltaQPlot::editPlot(const std::vector<std::string> &selectedItems)
{
    controller->editPlot(selectedItems);
}

void DeltaQPlot::updateSeries(QLineSeries *series, const QVector<QPointF> &data)
{
    series->replace(data);
}

void DeltaQPlot::updateXRange(double xRange)
{
    axisX->setRange(0, xRange);
}

std::vector<std::string> DeltaQPlot::getComponents()
{
    return controller->getComponents();
}

DQPlotList *DeltaQPlot::getPlotList()
{
    return plotList;
}

void DeltaQPlot::mousePressEvent(QMouseEvent *event)
{
    Q_EMIT plotSelected(this);
}
