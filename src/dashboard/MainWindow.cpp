#include "MainWindow.h"
#include "../Application.h"
#include "../maths/DeltaQOperations.h"
#include "DQPlotList.h"
#include "DeltaQPlot.h"
#include "NewPlotList.h"
#include "ObservableSettings.h"
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
    // Set up central widget and main layout
    centralWidget = new QWidget(this);
    setCentralWidget(centralWidget);
    mainLayout = new QHBoxLayout(centralWidget);

    // Configure scroll area for plots
    scrollArea = new QScrollArea(this);
    scrollArea->setWidgetResizable(true);
    scrollArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    scrollArea->setBaseSize(800, 800);

    // Set up plot container with grid layout
    plotContainer = new QWidget();
    plotLayout = new QGridLayout(plotContainer);
    scrollArea->setWidget(plotContainer);
    mainLayout->addWidget(scrollArea, 1);

    // Initialize side panels
    sidebar = new Sidebar(this);
    triggersTab = new TriggersTab(this);
    observableSettings = new ObservableSettings(this);
    stubWidget = new StubControlWidget(this);

    // Configure tabbed side panel
    sideTabWidget = new QTabWidget(this);
    sideTabWidget->addTab(sidebar, "System/Plots");
    sideTabWidget->addTab(observableSettings, "Probes settings");
    sideTabWidget->addTab(triggersTab, "Triggers");
    sideTabWidget->addTab(stubWidget, "Connection controls");
    sideTabWidget->setTabPosition(QTabWidget::West);
    // Connect sidebar signals
    connect(sidebar, &Sidebar::addPlotClicked, this, &MainWindow::onAddPlotClicked);

    // Set up side container layout
    sideContainer = new QWidget(this);
    sideLayout = new QVBoxLayout(sideContainer);
    sideLayout->addWidget(sideTabWidget);
    sideLayout->setStretch(1, 0); // Make tabs take up most space
    mainLayout->addWidget(sideContainer, 0);

    // Set up update timer in separate thread
    timerThread = new QThread(this);
    updateTimer = new QTimer();
    updateTimer->moveToThread(timerThread);
    connect(updateTimer, &QTimer::timeout, this, &MainWindow::updatePlots, Qt::QueuedConnection);
    connect(timerThread, &QThread::started, [this]() { updateTimer->start(200); });
    timerThread->start();

    // Register system reset observer
    Application::getInstance().addObserver([this] { this->reset(); });

    // Initialize time bounds
    auto now = std::chrono::system_clock::now();
    auto adjustedTime = now - std::chrono::milliseconds(200);
    timeLowerBound = std::chrono::duration_cast<std::chrono::nanoseconds>(adjustedTime.time_since_epoch()).count();

    // Connect sampling rate changes
    connect(sidebar, &Sidebar::onSamplingRateChanged, this, [this](int ms) {
        qDebug() << "MainWindow received sampling rate:" << ms;
        samplingRate = ms;
        QMetaObject::invokeMethod(updateTimer, [ms, this]() { updateTimer->setInterval(ms); }, Qt::QueuedConnection);
    });
}

/**
 * @brief Cleans up empty plots during reset.
 */
void MainWindow::reset()
{
    std::lock_guard<std::mutex> lock(plotDelMutex);
    auto it = plotContainers.begin();
    while (it != plotContainers.end()) {
        DeltaQPlot *plot = it.key();
        QWidget *plotWidget = it.value();

        if (plot->isEmptyAfterReset()) {
            it = plotContainers.erase(it);
            delete plot;
            if (plotWidget) {
                delete plotWidget;
            }
            sidebar->hideCurrentPlot();
        } else {
            ++it;
        }
    }
}

/**
 * @brief Updates all plots with new data from the system.
 */
void MainWindow::updatePlots()
{
    // Update time bounds
    timeLowerBound += std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::milliseconds(samplingRate)).count();
    uint64_t timeUpperBound = timeLowerBound + std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::milliseconds(samplingRate)).count();

    auto system = Application::getInstance().getSystem();
    std::lock_guard<std::mutex> lock(plotDelMutex);

    // Update probes
    for (auto &[name, probe] : system->getProbes()) {
        if (probe) {
            probe->getObservedDeltaQ(timeLowerBound, timeUpperBound);
            probe->calculateCalculatedDeltaQ(timeLowerBound, timeUpperBound);
        }
    }

    // Update operators
    for (auto &[name, op] : system->getOperators()) {
        if (op) {
            op->getObservedDeltaQ(timeLowerBound, timeUpperBound);
            op->calculateCalculatedDeltaQ(timeLowerBound, timeUpperBound);
        }
    }

    // Update outcomes
    for (auto &[name, outcome] : system->getOutcomes()) {
        if (outcome) {
            outcome->getObservedDeltaQ(timeLowerBound, timeUpperBound);
        }
    }

    // Update all plots
    for (auto [plot, _] : plotContainers.asKeyValueRange()) {
        plot->update(timeLowerBound, timeUpperBound);
    }
}

/**
 * @brief Destructor cleans up timer thread and resources.
 */
MainWindow::~MainWindow()
{
    if (timerThread) {
        timerThread->quit();
        timerThread->wait();
        delete timerThread;
    }
}

/**
 * @brief Adds a new plot based on sidebar selection.
 */
void MainWindow::onAddPlotClicked()
{
    auto selectedItems = sidebar->getPlotList()->getSelectedItems();

    if (selectedItems.empty()) {
        QMessageBox::warning(this, "No Selection", "Please select components before adding a plot.");
        return;
    }

    // Create new plot container
    auto *plotWidget = new QWidget(this);
    auto *plotWidgetLayout = new QVBoxLayout(plotWidget);
    auto *deltaQPlot = new DeltaQPlot(selectedItems, this);

    // Set up connections and tracking
    connect(deltaQPlot, &DeltaQPlot::plotSelected, this, &MainWindow::onPlotSelected);
    plotContainers[deltaQPlot] = plotWidget;

    // Configure layout
    plotWidgetLayout->addWidget(deltaQPlot);
    plotWidget->setMaximumWidth(scrollArea->width() / MAX_P_ROW);
    plotWidget->setMaximumHeight(scrollArea->height() / 2);

    // Position in grid
    int plotCount = plotContainers.size();
    int row = (plotCount - 1) / MAX_P_ROW;
    int col = (plotCount - 1) % MAX_P_COL;
    plotLayout->addWidget(plotWidget, row, col);

    // Update UI state
    onPlotSelected(deltaQPlot);
    sidebar->clearOnAdd();
}

/**
 * @brief Handles window resize events to adjust plot sizes.
 * @param event The resize event.
 */
void MainWindow::resizeEvent(QResizeEvent *event)
{
    QMainWindow::resizeEvent(event);
    for (auto [plot, widget] : plotContainers.asKeyValueRange()) {
        widget->setMaximumWidth(scrollArea->width() / MAX_P_COL);
        widget->setMaximumHeight(scrollArea->height() / 2);
    }
}

/**
 * @brief Shows context menu for plot management.
 * @param event The context menu event.
 */
void MainWindow::contextMenuEvent(QContextMenuEvent *event)
{
    QWidget *child = childAt(event->pos());
    if (!child)
        return;

    // Find which plot was right-clicked
    DeltaQPlot *selectedPlot = nullptr;
    for (auto it = plotContainers.begin(); it != plotContainers.end(); ++it) {
        if (it.value()->isAncestorOf(child)) {
            selectedPlot = it.key();
            break;
        }
    }

    if (!selectedPlot)
        return;

    // Create and show context menu
    QMenu contextMenu(this);
    QAction *removeAction = contextMenu.addAction("Remove Plot");

    QAction *selectedAction = contextMenu.exec(event->globalPos());

    if (selectedAction == removeAction) {
        onRemovePlot(selectedPlot);
    }
}

/**
 * @brief Removes a plot and cleans up resources.
 * @param plot The plot to remove.
 */
void MainWindow::onRemovePlot(DeltaQPlot *plot)
{
    QWidget *plotWidget = plotContainers.value(plot, nullptr);
    if (plotWidget) {
        plotLayout->removeWidget(plotWidget);
        plotWidget->deleteLater();
    }
    plotContainers.remove(plot);

    // Reorganize remaining plots
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

/**
 * @brief Updates sidebar when a plot is selected.
 * @param plot The newly selected plot.
 */
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
