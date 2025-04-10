

#ifndef SIDEBAR_H
#define SIDEBAR_H

#include "DQPlotList.h"
#include "NewPlotList.h"
#include <QLabel>
#include <QPushButton>
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

signals:
    void addPlotClicked();

private slots:
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
