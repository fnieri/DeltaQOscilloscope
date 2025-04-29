
#pragma once
#include "../diagram/System.h"
#include "DeltaQPlot.h"
#include "NewPlotList.h"
#include "Sidebar.h"
#include <QHBoxLayout>
#include <QListWidget>
#include <QMainWindow>
#include <QPushButton>
#include <QTimer>
#include <QVBoxLayout>
#include <qboxlayout.h>

#include "TriggersTab.h"

class MainWindow : public QMainWindow
{
    Q_OBJECT

    QHBoxLayout *mainLayout;

    QScrollArea *scrollArea;
    QGridLayout *plotLayout;

    QWidget *centralWidget;

    QThread *timerThread;
    QTimer *updateTimer;

    QTabWidget* sideTabWidget;  // NEW
    TriggersTab* triggersTab;   // NEW



    Sidebar *sidebar;
    QPushButton *addPlotButton;
    QMap<DeltaQPlot *, QWidget *> plotContainers; // Store plots dynamically
    uint64_t timeLowerBound;

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();
    void reset();

private Q_SLOTS:
    void updatePlots();
    void onUpdateSystem();
    void onAddPlotClicked();

    void onEditPlot(DeltaQPlot *plot);
    void onRemovePlot(DeltaQPlot *plot);
    void onPlotSelected(DeltaQPlot *plot);

protected:
    void contextMenuEvent(QContextMenuEvent *event) override;
    void resizeEvent(QResizeEvent *event) override;
};
