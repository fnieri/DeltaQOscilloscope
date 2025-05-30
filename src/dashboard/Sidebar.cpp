#include "Sidebar.h"

#include "NewPlotList.h"
#include "SamplingRateWidget.h"
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
    // Main layout setup
    layout = new QVBoxLayout(this);
    layout->setContentsMargins(0, 0, 0, 0);

    mainSplitter = new QSplitter(Qt::Vertical, this);

    // System creation section
    systemCreationWidget = new SystemCreationWidget(this);
    mainSplitter->addWidget(systemCreationWidget);

    // New plot section
    newPlotListWidget = new QWidget(this);
    newPlotListLayout = new QVBoxLayout(newPlotListWidget);
    newPlotListLayout->setContentsMargins(5, 5, 5, 5);

    newPlotLabel = new QLabel("Select probes for a new plot:", this);
    newPlotLabel->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    addNewPlotButton = new QPushButton("Add plot");
    newPlotList = new NewPlotList(this);
    connect(addNewPlotButton, &QPushButton::clicked, this, &Sidebar::onAddPlotClicked);

    newPlotListLayout->addWidget(newPlotLabel);
    newPlotListLayout->addWidget(newPlotList);
    newPlotListLayout->addWidget(addNewPlotButton);

    mainSplitter->addWidget(newPlotListWidget);

    // Current plot section (initially hidden)
    currentPlotWidget = new QWidget(this);
    currentPlotLayout = new QVBoxLayout(currentPlotWidget);
    currentPlotLabel = new QLabel("Modify current plot:", this);
    currentPlotLabel->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    currentPlotLabel->hide();

    currentPlotLayout->addWidget(currentPlotLabel);
    mainSplitter->addWidget(currentPlotWidget);

    // Sampling rate section
    samplingRateWidget = new SamplingRateWidget(this);
    mainSplitter->addWidget(samplingRateWidget);

    connect(samplingRateWidget, &SamplingRateWidget::onSamplingRateChanged, this, &Sidebar::handleSamplingRateChanged);
    layout->addWidget(mainSplitter);
}

/**
 * @brief Sets the current plot list widget to display.
 * @param plotList The DQPlotList widget to show in the current plot section.
 */
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

/**
 * @brief Handles sampling rate change events from the SamplingRateWidget.
 * @param ms The new sampling rate in milliseconds.
 */
void Sidebar::handleSamplingRateChanged(int ms)
{
    Q_EMIT onSamplingRateChanged(ms);
}

/**
 * @brief Hides the current plot management section.
 */
void Sidebar::hideCurrentPlot()
{
    if (currentPlotList) {
        layout->removeWidget(currentPlotList);
        currentPlotList = nullptr;
    }
    currentPlotLabel->hide();
}

/**
 * @brief Clears new plot selection after plot creation.
 */
void Sidebar::clearOnAdd()
{
    newPlotList->clearSelection();
}

/**
 * @brief Handles the "Add plot" button click event.
 * Validates selection and emits addPlotClicked() signal.
 */
void Sidebar::onAddPlotClicked()
{
    auto selectedItems = newPlotList->getSelectedItems();

    if (selectedItems.empty()) {
        QMessageBox::warning(this, "No Selection", "Please select probes before adding a plot.");
        return;
    }

    Q_EMIT addPlotClicked();
}
