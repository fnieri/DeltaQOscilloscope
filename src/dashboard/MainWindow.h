#pragma once
#include "../diagram/System.h"
#include "DeltaQPlot.h"
#include "NewPlotList.h"
#include "ObservableSettings.h"
#include "Sidebar.h"
#include "StubControlWidget.h"
#include "TriggersTab.h"

#include <QHBoxLayout>
#include <QListWidget>
#include <QMainWindow>
#include <QPushButton>
#include <QTimer>
#include <QVBoxLayout>
#include <qboxlayout.h>
#include <qwidget.h>

/**
 * @class MainWindow
 * @brief The main application window containing plots and control panels.
 */
class MainWindow : public QMainWindow
{
    Q_OBJECT

    QHBoxLayout *mainLayout; ///< Main horizontal layout

    QScrollArea *scrollArea; ///< Scroll area for plots
    QGridLayout *plotLayout; ///< Grid layout for plot arrangement
    QWidget *plotContainer; ///< Container widget for plots

    QWidget *centralWidget; ///< Central widget for main layout

    QThread *timerThread; ///< Thread for update timer
    QTimer *updateTimer; ///< Timer for periodic updates

    QWidget *sideContainer; ///< Container for side panels
    QVBoxLayout *sideLayout; ///< Layout for side panels
    QTabWidget *sideTabWidget; ///< Tab widget for side panels
    TriggersTab *triggersTab; ///< Triggers configuration panel
    Sidebar *sidebar; ///< Main sidebar control panel
    ObservableSettings *observableSettings; ///< Observable settings panel

    StubControlWidget *stubWidget; ///< Stub control widget (placeholder)
    QPushButton *addPlotButton; ///< Button to add new plots

    QMap<DeltaQPlot *, QWidget *> plotContainers; ///< Map of plots to their containers
    uint64_t timeLowerBound; ///< Lower time bound for data updates

    std::mutex plotDelMutex; ///< Mutex for plot deletion safety
    std::mutex updateMutex; ///< Mutex for update operations

    int samplingRate {200}; ///< Current sampling rate in milliseconds

public:
    MainWindow(QWidget *parent = nullptr);

    ~MainWindow();

    /**
     * @brief Resets the window state, cleaning up empty plots.
     */
    void reset();

private Q_SLOTS:
    /**
     * @brief Updates all plots with new data.
     */
    void updatePlots();

    /**
     * @brief Handles adding new plots from sidebar selection.
     */
    void onAddPlotClicked();

    /**
     * @brief Removes a specific plot.
     * @param plot The plot to remove.
     */
    void onRemovePlot(DeltaQPlot *plot);

    /**
     * @brief Handles plot selection changes.
     * @param plot The newly selected plot.
     */
    void onPlotSelected(DeltaQPlot *plot);

protected:
    /**
     * @brief Handles context menu events for plot management.
     * @param event The context menu event.
     */
    void contextMenuEvent(QContextMenuEvent *event) override;

    /**
     * @brief Handles window resize events to adjust plot sizes.
     * @param event The resize event.
     */
    void resizeEvent(QResizeEvent *event) override;
};
