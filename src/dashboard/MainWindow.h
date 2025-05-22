
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
class MainWindow : public QMainWindow
{
    Q_OBJECT

    QHBoxLayout *mainLayout;

    QScrollArea *scrollArea;
    QGridLayout *plotLayout;

    QWidget *plotContainer;

    QWidget *centralWidget;

    QThread *timerThread;
    QTimer *updateTimer;

    QWidget *sideContainer;
    QVBoxLayout *sideLayout;

    QTabWidget *sideTabWidget;
    TriggersTab *triggersTab;
    Sidebar *sidebar;
    ObservableSettings *observableSettings;

    StubControlWidget *stubWidget;

    QPushButton *addPlotButton;

    QMap<DeltaQPlot *, QWidget *> plotContainers;
    uint64_t timeLowerBound;

    std::mutex plotDelMutex;
    std::mutex updateMutex;

    int pollingRate {200};

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();
    void reset();

private Q_SLOTS:
    void updatePlots();
    void onAddPlotClicked();
    void onRemovePlot(DeltaQPlot *plot);
    void onPlotSelected(DeltaQPlot *plot);

protected:
    void contextMenuEvent(QContextMenuEvent *event) override;
    void resizeEvent(QResizeEvent *event) override;
};
