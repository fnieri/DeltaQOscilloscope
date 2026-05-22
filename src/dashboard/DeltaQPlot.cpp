#include "DeltaQPlot.h"
#include "ColorRegistry.h"
#include "CustomLegendPanel.h"
#include "DQPlotController.h"
#include "DQPlotList.h"
#include <QAreaSeries>
#include <QChartView>
#include <QHBoxLayout>
#include <QLineSeries>
#include <QMouseEvent>
#include <QToolButton>
#include <QVBoxLayout>

DeltaQPlot::DeltaQPlot(const std::vector<std::string> &selectedItems, QWidget *parent)
    : QWidget(parent)
{
    legendPanel = new CustomLegendPanel(this);

    QToolButton *toggleButton = new QToolButton(this);
    toggleButton->setText("> Legend");
    toggleButton->setCheckable(true);
    toggleButton->setChecked(true);
    connect(toggleButton, &QToolButton::toggled, legendPanel, &QWidget::setVisible);
    connect(toggleButton, &QToolButton::toggled, [toggleButton](bool checked) {
        toggleButton->setText(checked ? "^ Legend" : "> Legend");
    });

    QToolButton *boundsToggle = new QToolButton(this);
    boundsToggle->setText("Bounds: Area");
    boundsToggle->setCheckable(true);
    boundsToggle->setChecked(true);
    connect(boundsToggle, &QToolButton::toggled, [this, boundsToggle](bool checked) {
        boundsToggle->setText(checked ? "Bounds: Area" : "Bounds: Lines");
        controller->setBoundsAsArea(checked);
    });

    chart = new QChart();
    chartView = new QChartView(chart, this);
    chartView->setRenderHint(QPainter::Antialiasing);
    chart->legend()->setVisible(false);

    axisX = new QValueAxis();
    axisX->setTitleText("Delay (s)");
    axisX->setRange(0, 0.05);
    chart->addAxis(axisX, Qt::AlignBottom);

    axisY = new QValueAxis();
    axisY->setTitleText("ΔQ(x)");
    axisY->setRange(-0.01, 1.0);
    chart->addAxis(axisY, Qt::AlignLeft);

    controller = new DQPlotController(this, selectedItems);
    plotList = new DQPlotList(controller, this);

    QVBoxLayout *rightLayout = new QVBoxLayout();
    rightLayout->addWidget(toggleButton);
    rightLayout->addWidget(boundsToggle);
    rightLayout->addWidget(legendPanel);
    rightLayout->addStretch();

    QHBoxLayout *mainLayout = new QHBoxLayout(this);
    mainLayout->addWidget(chartView, 1);
    mainLayout->addLayout(rightLayout);
    mainLayout->setContentsMargins(0, 0, 0, 0);
    mainLayout->setSpacing(5);
    setLayout(mainLayout);
}

DeltaQPlot::~DeltaQPlot()
{
    delete controller;
    delete plotList;
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

void DeltaQPlot::addAreaSeries(QAreaSeries *series, const std::string &name, const QColor &color)
{
    chart->addSeries(series);
    series->setName(QString::fromStdString(name));
    series->attachAxis(axisX);
    series->attachAxis(axisY);

    QColor fill = color;
    fill.setAlphaF(0.15);
    QColor border = color;
    border.setAlphaF(0.5);

    series->setBrush(QBrush(fill));
    series->setPen(QPen(border, 1));
    series->setVisible(true);

    legendPanel->addEntry(QString::fromStdString(name), color);
}

void DeltaQPlot::removeSeries(QAbstractSeries *series)
{
    legendPanel->removeEntry(series->name());
    chart->removeSeries(series);
}

void DeltaQPlot::update(uint64_t timeLowerBound, uint64_t timeUpperBound)
{
    controller->update(timeLowerBound, timeUpperBound);
}

void DeltaQPlot::updateSeries(QLineSeries *series, const QVector<QPointF> &data)
{
    series->replace(data);
}

void DeltaQPlot::setSeriesVisible(QAbstractSeries *series, bool visible)
{
    series->setVisible(visible);
    legendPanel->setEntryVisible(series->name(), visible);
}

void DeltaQPlot::updateAreaSeries(QAreaSeries *series, const QVector<QPointF> &upper, const QVector<QPointF> &lower)
{
    series->upperSeries()->replace(upper);
    series->lowerSeries()->replace(lower);
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

void DeltaQPlot::editPlot(const std::vector<std::string> &selectedItems)
{
    controller->editPlot(selectedItems);
}

void DeltaQPlot::mousePressEvent(QMouseEvent *event)
{
    Q_EMIT plotSelected(this);
}
