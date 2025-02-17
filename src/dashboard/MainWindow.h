
#pragma once
#include "../diagram/System.h"
#include "DeltaQPlot.h"
#include <QHBoxLayout>
#include <QListWidget>
#include <QMainWindow>
#include <QPushButton>
#include <QTimer>
#include <QVBoxLayout>
#include <memory>
#include <qboxlayout.h>

class MainWindow : public QMainWindow
{
    Q_OBJECT

    QHBoxLayout *mainLayout;

    QVBoxLayout *plotLayout;
    QWidget *centralWidget;
    std::shared_ptr<System> system;
    QThread *timerThread;
    QTimer *updateTimer;
    QPushButton *addPlotButton;

    QMap<DeltaQPlot *, QWidget *> plotContainers; // Store plots dynamically
public:
    MainWindow(std::shared_ptr<System> system, QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void updatePlots();

    void onAddPlotClicked();
    void onEditPlot(DeltaQPlot *plot);
    void onRemovePlot(DeltaQPlot *plot);

protected:
    void contextMenuEvent(QContextMenuEvent *event) override;
};
