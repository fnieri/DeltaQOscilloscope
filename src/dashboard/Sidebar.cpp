
#include "Sidebar.h"

// @note Undef emit as it clashes with ANTLR emit!
// Must do it before ANTLR include!
#include "../parser/SystemParserInterface.h"

#include "../Application.h"
#include "NewPlotList.h"
#include <QBoxLayout>
#include <QFileDialog>
#include <QLabel>
#include <QMessageBox>
#include <iostream>
#include <memory>
#include <qboxlayout.h>
#include <qlabel.h>
#include <qlogging.h>
#include <qpushbutton.h>
#include <qtextedit.h>

Sidebar::Sidebar(QWidget *parent)
    : QWidget(parent)
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
    connect(updateSystemButton, &QPushButton::clicked, this, &Sidebar::onUpdateSystem);

    systemButtonsLayout->addWidget(saveSystemButton);
    connect(saveSystemButton, &QPushButton::clicked, this, &Sidebar::saveSystemTo);

    systemButtonsLayout->addWidget(loadSystemButton);
    connect(loadSystemButton, &QPushButton::clicked, this, &Sidebar::loadSystem);
    layout->addWidget(systemLabel);
    layout->addWidget(systemTextEdit);

    layout->addLayout(systemButtonsLayout);

    newPlotLabel = new QLabel("Add a new plot:", this);
    newPlotLabel->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    addNewPlotButton = new QPushButton("Add plot");
    newPlotList = new NewPlotList(this);
    connect(addNewPlotButton, &QPushButton::clicked, this, &Sidebar::onAddPlotClicked);

    layout->addWidget(newPlotLabel);
    layout->addWidget(newPlotList);
    layout->addWidget(addNewPlotButton);

    delaySettingsWidget = new DelaySettingsWidget(this);
    layout->addWidget(delaySettingsWidget);
    currentPlotLabel = new QLabel("Modify current plot:", this);
    currentPlotLabel->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    currentPlotLabel->hide(); // Initially hidden
    layout->addWidget(currentPlotLabel);
}
void Sidebar::setCurrentPlotList(DQPlotList *plotList)
{
    if (currentPlotList == plotList) {
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
}

void Sidebar::hideCurrentPlot()
{
    if (currentPlotList) {
        layout->removeWidget(currentPlotList);
        currentPlotList->deleteLater();
        currentPlotList = nullptr;
    }
    currentPlotLabel->hide();
}
void Sidebar::clearOnAdd()
{
    newPlotList->clearSelection();
}

void Sidebar::onUpdateSystem()
{
    std::string text = getSystemText();
    auto system = SystemParserInterface::parseString(text);

    if (system.has_value())
        Application::getInstance().setSystem(system.value());
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

void Sidebar::saveSystemTo()
{
    QFileDialog dialog(this);
    dialog.setFileMode(QFileDialog::AnyFile);
    QString filename = dialog.getSaveFileName(this, "Save file", "", "JSON Files (*.json)");

    if (!filename.isEmpty()) {
        std::string systemText = getSystemText();
        auto system = SystemParserInterface::parseString(systemText);

        if (system.has_value()) {
            std::ofstream outFile(filename.toStdString());
            if (outFile.is_open()) {
                outFile << systemText;
                outFile.close();
                QMessageBox::information(this, "Success", "File saved successfully.");
            } else {
                QMessageBox::critical(this, "Error", "Could not open file for writing.");
            }
        } else {
            QMessageBox::warning(this, "Error", "System parsing failed. File not saved.");
        }
    }
}

void Sidebar::loadSystem()
{
    QFileDialog dialog(this);
    std::string filename = dialog.getOpenFileName(this, "Select file", " ", ".json").toStdString();

    std::string text = getSystemText();
    auto system = SystemParserInterface::parseFile(filename);

    if (system.has_value())
        Application::getInstance().setSystem(system.value());
    std::string systemText = Application::getInstance().getSystem()->getSystemDefinitionText();
    systemTextEdit->setText(QString::fromStdString(systemText));
}
