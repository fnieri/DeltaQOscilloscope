

#ifndef SIDEBAR_H
#define SIDEBAR_H

#include "DQPlotList.h"
#include "DelaySettingsWidget.h"
#include "NewPlotList.h"
#include <QComboBox>
#include <QLabel>
#include <QPushButton>
#include <QSpinBox>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QWidget>
#include <qboxlayout.h>
class Sidebar : public QWidget
{
    Q_OBJECT

    QHBoxLayout *systemButtonsLayout;
    QLabel *systemLabel;
    QTextEdit *systemTextEdit;

    QPushButton *updateSystemButton;
    QPushButton *saveSystemButton;
    QPushButton *loadSystemButton;

    QLabel *newPlotLabel;
    NewPlotList *newPlotList;
    QPushButton *addNewPlotButton;

    QLabel *currentPlotLabel;
    DQPlotList *currentPlotList = nullptr;

    QVBoxLayout *layout;
    DelaySettingsWidget *delaySettingsWidget;
Q_SIGNALS:
    void addPlotClicked();

private Q_SLOTS:
    void onUpdateSystem();
    void saveSystemTo();
    void loadSystem();
    void onAddPlotClicked();

public:
    explicit Sidebar(QWidget *parent = nullptr);

    void setCurrentPlotList(DQPlotList *currentPlotList);
    void hideCurrentPlot();

    NewPlotList *getPlotList() const
    {
        return newPlotList;
    }

    std::string getSystemText() const
    {
        return systemTextEdit->toPlainText().toStdString();
    }
    void clearOnAdd();
};

#endif
