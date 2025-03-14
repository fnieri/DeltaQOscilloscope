
#include "MainWindow.h"
#include "../maths/DeltaQOperations.h"
#include "DQPlotList.h"
#include "DeltaQPlot.h"
#include "NewPlotList.h"
#include "Sidebar.h"
#include <QMenu>
#include <QMessageBox>
#include <QThread>
#include <QVBoxLayout>
#include <iostream>
MainWindow::MainWindow(std::shared_ptr<System> system, QWidget *parent)
    : QMainWindow(parent)
    , system(system)
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

    sidebar = new Sidebar(system, this);
    mainLayout->addWidget(sidebar, 0);

    connect(sidebar, &Sidebar::addPlotClicked, this, &MainWindow::onAddPlotClicked);

    timerThread = new QThread(this);
    updateTimer = new QTimer();
    updateTimer->moveToThread(timerThread);
    connect(updateTimer, &QTimer::timeout, this, &MainWindow::updatePlots, Qt::QueuedConnection);
    connect(timerThread, &QThread::started, [this]() { updateTimer->start(1000); });
    timerThread->start();
}
void MainWindow::updatePlots()
{
    system->calculateBinWidth();
    for (auto [plot, _] : plotContainers.asKeyValueRange()) {
        plot->update();
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
    auto *deltaQPlot = new DeltaQPlot(system, selectedItems, this);

    connect(deltaQPlot, &DeltaQPlot::plotSelected, this, &MainWindow::onPlotSelected);
    plotContainers[deltaQPlot] = plotWidget;

    plotWidgetLayout->addWidget(deltaQPlot);
    plotWidget->setMaximumWidth(scrollArea->width() / 4);
    plotWidget->setMaximumHeight(400);

    int plotCount = plotContainers.size();
    int maxPlotsPerRow = 4;
    int row = (plotCount - 1) / maxPlotsPerRow;
    int col = (plotCount - 1) % maxPlotsPerRow;

    plotLayout->addWidget(plotWidget, row, col);
    plotLayout->setColumnStretch(col, 1);
    plotLayout->setRowStretch(row, 1);

    onPlotSelected(deltaQPlot);
    sidebar->clearOnAdd();
}

void MainWindow::resizeEvent(QResizeEvent *event)
{
    QMainWindow::resizeEvent(event);
    for (auto [plot, widget] : plotContainers.asKeyValueRange()) {
        widget->setMaximumWidth(scrollArea->width() / 4);
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
    plot->update();

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
        plotLayout->removeWidget(plotWidget); // Remove the plot from the layout
        plotWidget->deleteLater();
        plotContainers.remove(plot);

        // Reorganize the remaining plots
        int plotCount = 0;
        for (auto it = plotContainers.begin(); it != plotContainers.end(); ++it) {
            int row = plotCount / 4; // Calculate the row
            int col = plotCount % 4; // Calculate the column
            plotLayout->addWidget(it.value(), row, col); // Re-add the plot to the grid

            // Update stretch factors for the row and column
            plotLayout->setRowStretch(row, 1);
            plotLayout->setColumnStretch(col, 1);

            plotCount++;
        }
    }
    sidebar->hideCurrentPlot();
}
void MainWindow::onPlotSelected(DeltaQPlot *plot)
{
    qDebug() << "onPlotSelected called for plot:" << plot;
    if (!plot)
        return;

    DQPlotList *plotList = plot->getPlotList();
    if (!plotList) {
        qDebug() << "No valid plot list found!";
        return;
    }

    sidebar->setCurrentPlotList(plotList);
    qDebug() << "Sidebar updated.";
}
