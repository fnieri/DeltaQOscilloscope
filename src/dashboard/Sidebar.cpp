
#include "Sidebar.h"
#include "../parser/ParserWrapper.h"
#include "NewPlotList.h"
#include <QBoxLayout>
#include <QLabel>
#include <QMessageBox>
#include <iostream>
#include <qboxlayout.h>
#include <qlabel.h>
#include <qlogging.h>
#include <qpushbutton.h>
#include <qtextedit.h>

Sidebar::Sidebar(std::shared_ptr<System> system, QWidget *parent)
    : QWidget(parent)
    , system(system)
{
    layout = new QVBoxLayout(this);
    layout->setAlignment(Qt::AlignTop);
    layout->setSpacing(10);
    layout->setContentsMargins(10, 10, 10, 10);

    systemLabel = new QLabel("Create or edit your system here");
    systemTextEdit = new QTextEdit();

    systemButtonsLayout = new QHBoxLayout();

    updateSystemButton = new QPushButton("Create or edit system");
    saveSystemButton = new QPushButton("Save system to");
    loadSystemButton = new QPushButton("Load system from");

    systemButtonsLayout->addWidget(updateSystemButton);
    systemButtonsLayout->addWidget(saveSystemButton);
    systemButtonsLayout->addWidget(loadSystemButton);

    layout->addWidget(systemLabel);
    layout->addWidget(systemTextEdit);

    layout->addLayout(systemButtonsLayout);

    connect(updateSystemButton, &QPushButton::clicked, this, &Sidebar::onUpdateSystem);

    newPlotLabel = new QLabel("Add a new plot:", this);
    newPlotLabel->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    addNewPlotButton = new QPushButton("Add plot");
    newPlotList = new NewPlotList(system, this);
    connect(addNewPlotButton, &QPushButton::clicked, this, &Sidebar::onAddPlotClicked); // Connect to onAddPlotClicked
    layout->addWidget(newPlotLabel);
    layout->addWidget(newPlotList);
    layout->addWidget(addNewPlotButton);

    currentPlotLabel = new QLabel("Modify current plot:", this);
    currentPlotLabel->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    currentPlotLabel->hide(); // Initially hidden
    layout->addWidget(currentPlotLabel);
}

void Sidebar::setCurrentPlotList(DQPlotList *plotList)
{
    if (!plotList) {
        qDebug() << "Sidebar: Received nullptr plotList, skipping update.";
        return;
    }

    if (currentPlotList == plotList) {
        qDebug() << "Sidebar: Skipping redundant update.";
        return;
    }

    if (currentPlotList) {
        if (!currentPlotList->parent()) {
            qDebug() << "Sidebar: currentPlotList is already deleted!";
        } else {
            layout->removeWidget(currentPlotList);
            currentPlotList->hide(); // <-- Avoids segfault if it's valid
        }
    }

    currentPlotList = plotList;
    layout->addWidget(currentPlotList);
    currentPlotList->show();
    currentPlotLabel->show();
    qDebug() << "Sidebar: Updated with new plot list.";
}

void Sidebar::hideCurrentPlot()
{
    if (currentPlotList) {
        layout->removeWidget(currentPlotList);
        currentPlotList->deleteLater(); // Properly delete the widget
        currentPlotList = nullptr;
    }
    currentPlotLabel->hide(); // Ensure label is hidden
}
void Sidebar::clearOnAdd()
{
    newPlotList->clearSelection();
}

void Sidebar::onUpdateSystem()
{
    std::string text = getSystemText();
    parseAndSaveJson(text);
}

void Sidebar::onAddPlotClicked()
{
    auto selectedItems = newPlotList->getSelectedItems();

    if (selectedItems.empty()) {
        QMessageBox::warning(this, "No Selection", "Please select components before adding a plot.");
        return;
    }

    emit addPlotClicked(); // Emit signal to MainWindow to add plot
}
