

#ifndef SIDEBAR_H
#define SIDEBAR_H

#include "DQPlotList.h"
#include "NewPlotList.h"
#include "SystemCreationWidget.h"
#include <QComboBox>
#include <QLabel>
#include <QPushButton>
#include <QSpinBox>
#include <QSplitter>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QWidget>
#include <qboxlayout.h>
class Sidebar : public QWidget
{
    Q_OBJECT

    QVBoxLayout *newPlotListLayout;
    QWidget *newPlotListWidget;
    QLabel *newPlotLabel;
    NewPlotList *newPlotList;
    QPushButton *addNewPlotButton;

    QWidget *currentPlotWidget;
    QVBoxLayout *currentPlotLayout;
    QLabel *currentPlotLabel;
    DQPlotList *currentPlotList = nullptr;

    QSplitter *mainSplitter;
    QVBoxLayout *layout;

    SystemCreationWidget *systemCreationWidget;
Q_SIGNALS:
    void addPlotClicked();

private Q_SLOTS:
    void onAddPlotClicked();

public:
    explicit Sidebar(QWidget *parent = nullptr);

    void setCurrentPlotList(DQPlotList *currentPlotList);
    void hideCurrentPlot();

    NewPlotList *getPlotList() const
    {
        return newPlotList;
    }

    void clearOnAdd();
};

#endif
