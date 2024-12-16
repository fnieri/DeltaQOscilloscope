//
// Created by francy on 04/12/24.
//

#include "Dashboard.h"
#include "ComponentEditDialog.h"
#include "JsonComponent.h"
#include <QComboBox>
#include <QMessageBox>
#include <fstream>
#include <qboxlayout.h>
#include <qlabel.h>
#include <qwidget.h>
Dashboard::~Dashboard()
{
}

Dashboard::Dashboard(QWidget *parent)
    : QMainWindow(parent)
{
    QWidget *centralWidget = new QWidget(this);
    QVBoxLayout *mainLayout = new QVBoxLayout;

    addComboBox();
    setUpLineEdits();

    // Setup form layout
    QVBoxLayout *formLayout = new QVBoxLayout;
    formLayout->addWidget(operatorChoice);
    formLayout->addWidget(nameLineEdit);
    formLayout->addWidget(startEventLineEdit);
    formLayout->addWidget(endEventLineEdit);

    addButton = new QPushButton("Add Component");
    formLayout->addWidget(addButton);

    setUpComponentTree();

    // Horizontal layout: form on the left, list on the right
    QHBoxLayout *horizontalLayout = new QHBoxLayout;
    horizontalLayout->addLayout(formLayout);
    horizontalLayout->addWidget(componentTree);

    mainLayout->addLayout(horizontalLayout);
    centralWidget->setLayout(mainLayout);

    setCentralWidget(centralWidget);

    // Connect the add button
    connect(addButton, &QPushButton::clicked, this, &Dashboard::onAdd);
}

void Dashboard::addComboBox()
{
    operatorChoice = new QComboBox;
    operatorChoice->addItems({"Outcome", "First-to-finish", "All-to-finish", "Probabilistic choice"});
}

void Dashboard::setUpComponentTree()
{
    componentTree = new QTreeWidget(this);
    componentTree->setColumnCount(1); // One column for component names
    componentTree->setHeaderLabel("Components");

    // Add categories as top-level items
    auto *outcomeCategory = new QTreeWidgetItem(componentTree, QStringList("Outcome"));
    auto *firstToFinishCategory = new QTreeWidgetItem(componentTree, QStringList("First-to-finish"));
    auto *allToFinishCategory = new QTreeWidgetItem(componentTree, QStringList("All-to-finish"));
    auto *probabilisticCategory = new QTreeWidgetItem(componentTree, QStringList("Probabilistic choice"));

    componentTree->addTopLevelItem(outcomeCategory);
    componentTree->addTopLevelItem(firstToFinishCategory);
    componentTree->addTopLevelItem(allToFinishCategory);
    componentTree->addTopLevelItem(probabilisticCategory);

    // Allow selecting only child items
    componentTree->setSelectionMode(QAbstractItemView::SingleSelection);
    connect(componentTree, &QTreeWidget::itemDoubleClicked, this, &Dashboard::onEditItem);
}
void Dashboard::setUpLineEdits()
{
    QVBoxLayout *nameLayout = new QVBoxLayout;
    QVBoxLayout *startEventLayout = new QVBoxLayout;
    QVBoxLayout *endEventLayout = new QVBoxLayout;

    nameWidget = new QWidget;
    startEventWidget = new QWidget;
    endEventWidget = new QWidget;

    nameLabel = new QLabel("Operator's name");
    startEventLabel = new QLabel("Start event");
    endEventLabel = new QLabel("End event");

    nameLineEdit = new QLineEdit;
    startEventLineEdit = new QLineEdit;
    endEventLineEdit = new QLineEdit;

    nameLineEdit->setPlaceholderText("Name");
    startEventLineEdit->setPlaceholderText("Start Event");
    endEventLineEdit->setPlaceholderText("End Event");

    nameLayout->addWidget(nameLabel);
    nameLayout->addWidget(nameLineEdit);
    nameWidget->setLayout(nameLayout);

    startEventLayout->addWidget(startEventLabel);
    startEventLayout->addWidget(startEventLineEdit);
    startEventWidget->setLayout(startEventLayout);

    endEventLayout->addWidget(endEventLabel);
    endEventLayout->addWidget(endEventLineEdit);
    endEventWidget->setLayout(endEventLayout);
}

void Dashboard::setupAddButton()
{
    addButton = new QPushButton("Submit");
    connect(addButton, &QPushButton::clicked, this, &Dashboard::onAdd);
}

void Dashboard::onAdd()
{

    QString name = nameLineEdit->text().trimmed();
    if (!areFieldsValid(name.toStdString())) {
        return;
    }

    QString startEvent = startEventLineEdit->text().trimmed();
    QString endEvent = endEventLineEdit->text().trimmed();
    QString operatorType = operatorChoice->currentText();

    // Map operator type to short code
    std::string operatorShortCode;
    QTreeWidgetItem *parentCategory = nullptr;

    if (operatorType == "Outcome") {
        operatorShortCode = "O";
        parentCategory = componentTree->topLevelItem(0); // Outcome category
    } else if (operatorType == "First-to-finish") {
        operatorShortCode = "F";
        parentCategory = componentTree->topLevelItem(1); // First-to-finish category
    } else if (operatorType == "All-to-finish") {
        operatorShortCode = "A";
        parentCategory = componentTree->topLevelItem(2); // All-to-finish category
    } else if (operatorType == "Probabilistic choice") {
        operatorShortCode = "P";
        parentCategory = componentTree->topLevelItem(3); // Probabilistic choice category
    }

    JsonComponent jsonComponent {name.toStdString(), startEvent.toStdString(), endEvent.toStdString(), operatorShortCode};

    componentsJson.push_back(jsonComponent);

    // Add as child item under the appropriate category
    if (parentCategory) {
        auto *childItem = new QTreeWidgetItem(parentCategory, QStringList(QString::fromStdString(name.toStdString())));
        parentCategory->addChild(childItem);
    }

    // Success and reset
    QMessageBox::information(this, "Success", "Component added successfully!");
    nameLineEdit->clear();
    startEventLineEdit->clear();
    endEventLineEdit->clear();
    operatorChoice->setCurrentIndex(0);
}
void Dashboard::onEditItem(QTreeWidgetItem *item, int column)
{
    if (!item || !item->parent()) {
        // Ignore clicks on category headers
        return;
    }

    QString itemName = item->text(0);

    // Find the corresponding component in componentsJson
    auto it = std::find_if(
        componentsJson.begin(), componentsJson.end(), [&](const JsonComponent &component) { return QString::fromStdString(component.name) == itemName; });

    if (it == componentsJson.end())
        return;

    JsonComponent &component = *it;
    std::string oldName = component.name;
    // Open the edit dialog
    ComponentEditDialog dialog(this);
    dialog.setFields(component);

    if (dialog.exec() == QDialog::Accepted) {
        JsonComponent editedComponent = dialog.getEditedComponent();

        if (editedComponent.name.empty() || editedComponent.startEvent.empty() || (editedComponent.endEvent.empty())) {
            QMessageBox::warning(this, "Validation Error", "Please fill in the required fields.");
            return;
        }

        std::string newName = editedComponent.name;
        if (oldName != newName) {
            // Check for duplicates in the existing components (excluding the current one being edited)
            for (const auto &jsonComponent : componentsJson) {
                if (newName == jsonComponent.name) {
                    QMessageBox::warning(this, "Validation Error", "A component with this name already exists.");
                    return;
                }
            }
        }

        bool typeChanged = (component.type != editedComponent.type);

        component = editedComponent;

        if (typeChanged) {
            // Remove the item from the current category
            QTreeWidgetItem *parentCategory = item->parent();
            if (parentCategory) {
                parentCategory->removeChild(item);
            }

            // Add the item to the new category
            QTreeWidgetItem *newCategory = nullptr;
            if (editedComponent.type == "O") {
                newCategory = componentTree->topLevelItem(0); // Outcome category
            } else if (editedComponent.type == "F") {
                newCategory = componentTree->topLevelItem(1); // First-to-finish category
            } else if (editedComponent.type == "A") {
                newCategory = componentTree->topLevelItem(2); // All-to-finish category
            } else if (editedComponent.type == "P") {
                newCategory = componentTree->topLevelItem(3); // Probabilistic choice category
            }

            if (newCategory) {
                newCategory->addChild(item);
            }
        }

        // Update the item's text (name may have changed)
        item->setText(0, QString::fromStdString(editedComponent.name));

        QMessageBox::information(this, "Success", "Component updated successfully!");
    }
}

bool Dashboard::areFieldsValid(const std::string &name)
{
    for (const auto &jsonComponent : componentsJson) {
        if (name == jsonComponent.name) {
            QMessageBox::warning(this, "Validation Error", "A component with this name already exists.");
            return false;
        }
    }

    // Reset styles for all fields
    auto resetField = [](QLineEdit *field) { field->setStyleSheet(""); };
    resetField(nameLineEdit);
    resetField(startEventLineEdit);
    resetField(endEventLineEdit);

    // Validate fields
    bool isValid = true;

    if (nameLineEdit->text().trimmed().isEmpty()) {
        nameLineEdit->setStyleSheet("background-color: #FFCCCC;"); // Highlight in red
        isValid = false;
    }

    if (startEventLineEdit->text().trimmed().isEmpty()) {
        startEventLineEdit->setStyleSheet("background-color: #FFCCCC;");
        isValid = false;
    }

    if (operatorChoice->currentText() == "Outcome" && endEventLineEdit->text().trimmed().isEmpty()) {
        endEventLineEdit->setStyleSheet("background-color: #FFCCCC;");
        isValid = false;
    }

    if (!isValid) {
        QMessageBox::warning(this, "Validation Error", "Please fill in the required fields.");
    }

    return isValid;
}
