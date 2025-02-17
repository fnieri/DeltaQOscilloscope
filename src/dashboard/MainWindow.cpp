
#include "MainWindow.h"
#include "../maths/DeltaQOperations.h"
#include "AddPlotDialog.h"
#include "DeltaQPlot.h"
#include <QMenu>
#include <QMessageBox>
#include <QThread>
#include <qboxlayout.h>

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

    timerThread = new QThread(this); // Store in the class so we can clean up later
    updateTimer = new QTimer();
    updateTimer->moveToThread(timerThread);
    connect(updateTimer, &QTimer::timeout, this, &MainWindow::updatePlots, Qt::QueuedConnection);
    connect(timerThread, &QThread::started, [this]() { updateTimer->start(500); });
    timerThread->start();

    addPlotButton = new QPushButton("Add Plot", this);
    connect(addPlotButton, &QPushButton::clicked, this, &MainWindow::onAddPlotClicked);
    mainLayout->addWidget(addPlotButton);
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
    AddPlotDialog dialog(system, {}, this);
    if (dialog.exec() == QDialog::Accepted) {
        SelectionResult selection = dialog.getSelections();

        // Determine the operation function pointer
        DeltaQ (*operationFunction)(const std::vector<DeltaQ> &) = nullptr;
        if (selection.selectedOperation == "Convolution") {
            operationFunction = &convolveN;
        } else if (selection.selectedOperation == "All-to-Finish") {
            operationFunction = &allToFinish;
        } else if (selection.selectedOperation == "First-to-Finish") {
            operationFunction = &firstToFinish;
        }

        // Create new DeltaQPlot with operation function pointer
        auto *plotWidget = new QWidget(this);
        auto *plotLayout = new QVBoxLayout(plotWidget);
        auto *deltaQPlot = new DeltaQPlot(system, this, operationFunction);
        plotContainers[deltaQPlot] = plotWidget;

        // Add outcomes
        for (const QString &item : selection.selectedOutcomes) {
            deltaQPlot->addComponent(item.toStdString(), false);
        }

        // Add probes
        for (const QString &item : selection.selectedProbes) {
            deltaQPlot->addComponent(item.toStdString(), true);
        }

        plotLayout->addWidget(deltaQPlot);
        mainLayout->addWidget(plotWidget);
    }
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

void MainWindow::onEditPlot(DeltaQPlot *plot)
{
    // Get currently plotted items
    QStringList existingItems;
    for (const auto &name : plot->getComponents()) {
        existingItems.append(QString::fromStdString(name));
    }

    // Open dialog with preselected items
    AddPlotDialog dialog(system, existingItems, this);
    if (dialog.exec() == QDialog::Accepted) {
        SelectionResult selection = dialog.getSelections();

        // Determine new operation function pointer
        DeltaQ (*operationFunction)(const std::vector<DeltaQ> &) = nullptr;
        if (selection.selectedOperation == "Convolution") {
            operationFunction = &convolveN;
        } else if (selection.selectedOperation == "All-to-Finish") {
            operationFunction = &allToFinish;
        } else if (selection.selectedOperation == "First-to-Finish") {
            operationFunction = &firstToFinish;
        }

        // Update the plot's operation function
        plot->setOperation(operationFunction);

        // Remove unselected items
        for (const QString &name : existingItems) {
            if (!selection.selectedOutcomes.contains(name) && !selection.selectedProbes.contains(name)) {
                plot->removeComponent(name.toStdString());
            }
        }

        // Add new selections
        for (const QString &name : selection.selectedOutcomes) {
            std::string stdName = name.toStdString();
            if (!plot->containsComponent(stdName)) {
                plot->addComponent(stdName, false);
            }
        }
        for (const QString &name : selection.selectedProbes) {
            std::string stdName = name.toStdString();
            if (!plot->containsComponent(stdName)) {
                plot->addComponent(stdName, true);
            }
        }

        plot->update();
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
}
