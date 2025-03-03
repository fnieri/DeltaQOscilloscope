
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

    std::shared_ptr<System> system;

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
    DQPlotList *currentPlotList = nullptr; // @note Need to be nullptr because it will be site at compile time to gibberish

    QVBoxLayout *layout;

signals:
    void addPlotClicked(); // Add a signal for the plot addition

private slots:
    void onUpdateSystem();
    void saveSystemTo();
    void loadSystem();
    void onAddPlotClicked(); // Handle the plot creation

public:
    explicit Sidebar(std::shared_ptr<System> system, QWidget *parent = nullptr);

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
