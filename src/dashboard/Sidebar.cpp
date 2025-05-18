
#include "Sidebar.h"

#include "NewPlotList.h"
#include "SystemCreationWidget.h"
#include <QBoxLayout>
#include <QFileDialog>
#include <QLabel>
#include <QMessageBox>
#include <iostream>
#include <qboxlayout.h>
#include <qlabel.h>
#include <qlogging.h>
#include <qnamespace.h>
#include <qpushbutton.h>
#include <qsplitter.h>
#include <qtextedit.h>

Sidebar::Sidebar(QWidget *parent)
    : QWidget(parent)
{
    layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    mainSplitter = new QSplitter(Qt::Vertical, this);

    systemCreationWidget = new SystemCreationWidget(this);
    mainSplitter->addWidget(systemCreationWidget);

    newPlotListWidget = new QWidget(this);
    newPlotListLayout = new QVBoxLayout(newPlotListWidget);
    newPlotListLayout->setContentsMargins(5, 5, 5, 5);

    newPlotLabel = new QLabel("Add a new plot:", this);
    newPlotLabel->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    addNewPlotButton = new QPushButton("Add plot");
    newPlotList = new NewPlotList(this);
    connect(addNewPlotButton, &QPushButton::clicked, this, &Sidebar::onAddPlotClicked);

    newPlotListLayout->addWidget(newPlotLabel);
    newPlotListLayout->addWidget(newPlotList);
    newPlotListLayout->addWidget(addNewPlotButton);

    mainSplitter->addWidget(newPlotListWidget);

    currentPlotWidget = new QWidget(this);
    currentPlotLayout = new QVBoxLayout(currentPlotWidget);
    currentPlotLabel = new QLabel("Modify current plot:", this);
    currentPlotLabel->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    currentPlotLabel->hide();

    currentPlotLayout->addWidget(currentPlotLabel);
    mainSplitter->addWidget(currentPlotWidget);

    layout->addWidget(mainSplitter);
}

void Sidebar::setCurrentPlotList(DQPlotList *plotList)
{
    if (currentPlotList == plotList) {
        return;
    }

    if (currentPlotList) {
        layout->removeWidget(currentPlotList);
        currentPlotList->hide();
    }

    if (plotList) {
        currentPlotList = plotList;
        layout->addWidget(currentPlotList);
        currentPlotList->show();
        currentPlotLabel->show();
    }
}

void Sidebar::hideCurrentPlot()
{
    if (currentPlotList) {
        layout->removeWidget(currentPlotList);
        currentPlotList = nullptr;
    }
    currentPlotLabel->hide();
}

void Sidebar::clearOnAdd()
{
    newPlotList->clearSelection();
}

void Sidebar::onAddPlotClicked()
{
    auto selectedItems = newPlotList->getSelectedItems();

    if (selectedItems.empty()) {
        QMessageBox::warning(this, "No Selection", "Please select components before adding a plot.");
        return;
    }

    Q_EMIT addPlotClicked();
}
