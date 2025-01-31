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

#include <QFileDialog>
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

    QPushButton *saveButton = new QPushButton("Save");
    QPushButton *loadButton = new QPushButton("Load");
    mainLayout->addWidget(saveButton);
    mainLayout->addWidget(loadButton);

    // Connect signals
    connect(saveButton, &QPushButton::clicked, this, &Dashboard::saveToFile);
    connect(loadButton, &QPushButton::clicked, this, &Dashboard::loadFromFile);
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
    isModified = true;
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
        isModified = true;
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

void Dashboard::saveToFile()
{
    // Check if the file is already saved, otherwise prompt for a filename
    if (currentFileName.isEmpty()) {
        currentFileName = QFileDialog::getSaveFileName(this, "Save File", "", "JSON Files (*.json)");
        if (currentFileName.isEmpty()) {
            return; // User canceled the dialog
        }
    }

    // Convert components to JSON
    nlohmann::json jsonData;
    jsonData["components"] = nlohmann::json::array();
    for (const auto &component : componentsJson) {
        jsonData["components"].push_back(component.toJson());
    }

    // Write JSON to the file
    try {
        std::ofstream file(currentFileName.toStdString());
        if (!file.is_open()) {
            throw std::ios::failure("Failed to open file");
        }
        file << jsonData.dump(4); // Pretty-print with 4 spaces
        file.close();

        isModified = false; // Mark as saved
        updateWindowTitle();
        QMessageBox::information(this, "Success", "File saved successfully!");
    } catch (const std::exception &e) {
        QMessageBox::critical(this, "Error", "Failed to save file: " + QString::fromStdString(e.what()));
    }
}

void Dashboard::loadFromFile()
{
    QString fileName = QFileDialog::getOpenFileName(this, "Open JSON File", "", "JSON Files (*.json)");
    if (fileName.isEmpty())
        return;

    std::ifstream file(fileName.toStdString());
    if (!file.is_open()) {
        QMessageBox::critical(this, "Error", "Failed to open the file.");
        return;
    }

    try {
        nlohmann::json json;
        file >> json;

        if (!json.contains("components") || !json["components"].is_array()) {
            QMessageBox::warning(this, "Invalid File", "The selected file does not have a valid components array.");
            return;
        }

        componentsJson.clear();
        componentTree->clear();

        // Add all categories before loading
        addAllCategoriesToTree();

        for (const auto &componentJson : json["components"]) {
            JsonComponent component;
            component.name = componentJson.at("name").get<std::string>();
            component.type = componentJson.at("type").get<std::string>();
            component.startEvent = componentJson.at("start").get<std::string>();
            component.endEvent = componentJson.at("end").get<std::string>();

            componentsJson.push_back(component);
            addComponentToTree(component);
        }

        currentFileName = fileName;
        setWindowTitle(QFileInfo(currentFileName).fileName());
        file.close();

        isModified = false; // Reset modification state
    } catch (const std::exception &e) {
        QMessageBox::critical(this, "Error", QString("An error occurred while loading the file: %1").arg(e.what()));
    }
}

void Dashboard::closeEvent(QCloseEvent *event)
{
    if (isModified) {
        QMessageBox::StandardButton reply = QMessageBox::question(this, "Unsaved Changes", "You have unsaved changes. Do you want to save them before exiting?",
            QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel);

        if (reply == QMessageBox::Yes) {
            saveToFile();
            event->accept();
        } else if (reply == QMessageBox::No) {
            event->accept();
        } else {
            event->ignore(); // Cancel the close
        }
    } else {
        event->accept(); // No changes, proceed with close
    }
}

void Dashboard::updateWindowTitle()
{
    QString title = currentFileName.isEmpty() ? "[Untitled system]" : QFileInfo(currentFileName).fileName();
    setWindowTitle(title + " - Component Dashboard");
}

void Dashboard::addComponentToTree(const JsonComponent &component)
{
    // Determine the parent item based on the component type
    QString typeCategory;
    if (component.type == "O") {
        typeCategory = "Outcome";
    } else if (component.type == "F") {
        typeCategory = "First-to-finish";
    } else if (component.type == "P") {
        typeCategory = "Probabilistic choice";
    } else if (component.type == "A") {
        typeCategory = "All-to-finish";
    } else {
        typeCategory = "Unknown"; // Fallback for unexpected types
    }

    // Find the parent item in the tree
    QTreeWidgetItem *parentItem = nullptr;
    for (int i = 0; i < componentTree->topLevelItemCount(); ++i) {
        QTreeWidgetItem *item = componentTree->topLevelItem(i);
        if (item->text(0) == typeCategory) {
            parentItem = item;
            break;
        }
    }

    // If the parent item doesn't exist, create it
    if (!parentItem) {
        parentItem = new QTreeWidgetItem(componentTree);
        parentItem->setText(0, typeCategory);
        componentTree->addTopLevelItem(parentItem);
    }

    // Create a new item for the component and add it to the parent
    QTreeWidgetItem *componentItem = new QTreeWidgetItem();
    componentItem->setText(0, QString::fromStdString(component.name)); // Display the name
    componentItem->setData(0, Qt::UserRole, QVariant::fromValue(static_cast<int>(componentsJson.size() - 1)));
    parentItem->addChild(componentItem);

    // Expand the parent item to make the new component visible
    parentItem->setExpanded(true);
}

void Dashboard::addAllCategoriesToTree()
{
    const QStringList categories = {"Outcome", "First-to-finish", "All-to-finish", "Probabilistic choice"};

    for (const QString &category : categories) {
        // Check if the category already exists in the tree
        bool categoryExists = false;
        for (int i = 0; i < componentTree->topLevelItemCount(); ++i) {
            if (componentTree->topLevelItem(i)->text(0) == category) {
                categoryExists = true;
                break;
            }
        }

        // Add the category if it doesn't exist
        if (!categoryExists) {
            QTreeWidgetItem *categoryItem = new QTreeWidgetItem(componentTree);
            categoryItem->setText(0, category);
            componentTree->addTopLevelItem(categoryItem);
        }
    }
}
