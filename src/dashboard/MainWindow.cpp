#include "MainWindow.h"
#include "../Application.h"
#include "../maths/DeltaQOperations.h"
#include "DQPlotList.h"
#include "DeltaQPlot.h"
#include "NewPlotList.h"
#include "Sidebar.h"
#include <QMenu>
#include <QMessageBox>
#include <QThread>
#include <QVBoxLayout>

#define MAX_P_ROW 2
#define MAX_P_COL 2

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
{
    centralWidget = new QWidget(this);
    setCentralWidget(centralWidget);
    mainLayout = new QHBoxLayout(centralWidget);

    // Create a scroll area for the plots
    scrollArea = new QScrollArea(this);
    scrollArea->setWidgetResizable(true); // Allow the widget to resize

    scrollArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    scrollArea->setBaseSize(800, 800);
    QWidget *plotContainer = new QWidget();
    plotLayout = new QGridLayout(plotContainer);
    scrollArea->setWidget(plotContainer);

    mainLayout->addWidget(scrollArea, 1);

    sidebar = new Sidebar(this);
    triggersTab = new TriggersTab(this);

    // Tab widget to hold sidebar and triggers tab
    sideTabWidget = new QTabWidget(this);
    sideTabWidget->addTab(sidebar, "Sidebar");
    sideTabWidget->addTab(triggersTab, "Triggers");
    sideTabWidget->setTabPosition(QTabWidget::West); // Tabs on the left
    mainLayout->addWidget(sideTabWidget, 0);
    connect(sidebar, &Sidebar::addPlotClicked, this, &MainWindow::onAddPlotClicked);

    timerThread = new QThread(this);
    updateTimer = new QTimer();
    updateTimer->moveToThread(timerThread);
    connect(updateTimer, &QTimer::timeout, this, &MainWindow::updatePlots, Qt::QueuedConnection);
    connect(timerThread, &QThread::started, [this]() { updateTimer->start(200); });
    timerThread->start();
    Application::getInstance().addObserver([this] { this->reset(); });
    auto now = std::chrono::system_clock::now();
    auto adjustedTime = now - std::chrono::milliseconds(1000);
    timeLowerBound = std::chrono::duration_cast<std::chrono::nanoseconds>(adjustedTime.time_since_epoch()).count();
}

void MainWindow::reset()
{
    sidebar->hideCurrentPlot();

    for (auto it = plotContainers.begin(); it != plotContainers.end(); ++it) {
        DeltaQPlot *plot = it.key();
        QWidget *plotWidget = it.value();
        if (plot) {
            delete plot;
            plot = nullptr;
        }
        if (plotWidget) {
            delete plotWidget;
            plotWidget = nullptr;
        }
    }
    plotContainers.clear();
}


void MainWindow::updatePlots()
{
    timeLowerBound += std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::milliseconds(200)).count();
    uint64_t timeUpperBound = timeLowerBound + std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::milliseconds(200)).count();
    auto system = Application::getInstance().getSystem();

    for (auto [plot, _] : plotContainers.asKeyValueRange()) {
        plot->update(timeLowerBound, timeUpperBound);
    }
}

MainWindow::~MainWindow()
{
    if (timerThread) {
        timerThread->quit();
        timerThread->wait();
        delete timerThread;
    }
}
void MainWindow::onAddPlotClicked()
{
    auto selectedItems = sidebar->getPlotList()->getSelectedItems();

    if (selectedItems.empty()) {
        QMessageBox::warning(this, "No Selection", "Please select components before adding a plot.");
        return;
    }

    auto *plotWidget = new QWidget(this);
    auto *plotWidgetLayout = new QVBoxLayout(plotWidget);
    auto *deltaQPlot = new DeltaQPlot(selectedItems, this);

    connect(deltaQPlot, &DeltaQPlot::plotSelected, this, &MainWindow::onPlotSelected);
    plotContainers[deltaQPlot] = plotWidget;

    plotWidgetLayout->addWidget(deltaQPlot);

    plotWidget->setMaximumWidth(scrollArea->width() / MAX_P_ROW);
    plotWidget->setMaximumHeight(scrollArea->height() / 2);

    int plotCount = plotContainers.size();
    int row = (plotCount - 1) / MAX_P_ROW;
    int col = (plotCount - 1) % MAX_P_COL;

    plotLayout->addWidget(plotWidget, row, col);

    onPlotSelected(deltaQPlot);
    sidebar->clearOnAdd();
}

void MainWindow::resizeEvent(QResizeEvent *event)
{
    QMainWindow::resizeEvent(event);
    for (auto [plot, widget] : plotContainers.asKeyValueRange()) {
        widget->setMaximumWidth(scrollArea->width() / MAX_P_COL);
        widget->setMaximumHeight(scrollArea->height() / 2);
    }
}
void MainWindow::onUpdateSystem()
{
    std::string text = sidebar->getSystemText();
}

void MainWindow::onEditPlot(DeltaQPlot *plot)
{
    auto selectedItems = sidebar->getPlotList()->getSelectedItems();
    plot->editPlot(selectedItems);

    onPlotSelected(plot);
}

void MainWindow::contextMenuEvent(QContextMenuEvent *event)
{
    QWidget *child = childAt(event->pos());
    if (!child)
        return;

    DeltaQPlot *selectedPlot = nullptr;
    for (auto it = plotContainers.begin(); it != plotContainers.end(); ++it) {
        if (it.value()->isAncestorOf(child)) {
            selectedPlot = it.key();
            break;
        }
    }

    if (!selectedPlot)
        return;

    QMenu contextMenu(this);
    QAction *editAction = contextMenu.addAction("Edit Plot");
    QAction *removeAction = contextMenu.addAction("Remove Plot");

    QAction *selectedAction = contextMenu.exec(event->globalPos());

    if (selectedAction == editAction) {
        onEditPlot(selectedPlot);
    } else if (selectedAction == removeAction) {
        onRemovePlot(selectedPlot);
    }
}

void MainWindow::onRemovePlot(DeltaQPlot *plot)
{
    QWidget *plotWidget = plotContainers.value(plot, nullptr);
    if (plotWidget) {
        plotLayout->removeWidget(plotWidget);
        plotWidget->deleteLater();
    }
    plotContainers.remove(plot);
    int plotCount = 0;
    for (auto it = plotContainers.begin(); it != plotContainers.end(); ++it) {
        int row = plotCount / MAX_P_ROW;
        int col = plotCount % MAX_P_COL;
        plotLayout->addWidget(it.value(), row, col);
        plotLayout->setRowStretch(row, 1);
        plotLayout->setColumnStretch(col, 1);
        plotCount++;
    }

    sidebar->hideCurrentPlot();
}

void MainWindow::onPlotSelected(DeltaQPlot *plot)
{
    if (!plot)
        return;
    DQPlotList *plotList = plot->getPlotList();
    if (!plotList) {
        return;
    }
    sidebar->setCurrentPlotList(plotList);
}
