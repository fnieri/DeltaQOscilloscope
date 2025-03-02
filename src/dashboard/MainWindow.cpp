
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
    plotLayout = new QVBoxLayout();
    QWidget *plotContainer = new QWidget();
    plotContainer->setLayout(plotLayout);
    mainLayout->addWidget(plotContainer);
    sidebar = new Sidebar(system, this);
    mainLayout->addWidget(sidebar);

    // Connect the signal from Sidebar to the slot in MainWindow
    connect(sidebar, &Sidebar::addPlotClicked, this, &MainWindow::onAddPlotClicked);

    timerThread = new QThread(this);
    updateTimer = new QTimer();
    updateTimer->moveToThread(timerThread);
    connect(updateTimer, &QTimer::timeout, this, &MainWindow::updatePlots, Qt::QueuedConnection);
    connect(timerThread, &QThread::started, [this]() { updateTimer->start(500); });
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
    auto *plotLayout = new QVBoxLayout(plotWidget);
    auto *deltaQPlot = new DeltaQPlot(system, selectedItems, this);

    connect(deltaQPlot, &DeltaQPlot::plotSelected, this, &MainWindow::onPlotSelected);
    plotContainers[deltaQPlot] = plotWidget;

    plotLayout->addWidget(deltaQPlot);
    mainLayout->addWidget(plotWidget);

    onPlotSelected(deltaQPlot);

    sidebar->clearOnAdd();
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
        plotLayout->removeWidget(plotWidget);
        plotWidget->deleteLater();
        plotContainers.remove(plot);
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
