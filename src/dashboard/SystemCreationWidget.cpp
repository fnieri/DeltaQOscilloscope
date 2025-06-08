#include "SystemCreationWidget.h"
#include "../Application.h"
#include "../diagram/SystemUtils.h"
#include "../parser/SystemParserInterface.h"
#include <QFileDialog>
#include <QMessageBox>
#include <fstream>

/**
 * @brief Constructs the SystemCreationWidget and sets up the UI.
 * @param parent Parent QWidget.
 */
SystemCreationWidget::SystemCreationWidget(QWidget *parent)
    : QWidget(parent)
{
    mainLayout = new QVBoxLayout(this);
    mainLayout->setAlignment(Qt::AlignTop);
    mainLayout->setSpacing(10);
    mainLayout->setContentsMargins(0, 0, 0, 0);

    systemLabel = new QLabel("Create or edit your system here");
    systemTextEdit = new QTextEdit();

    buttonLayout = new QHBoxLayout();
    updateSystemButton = new QPushButton("Create or edit system");
    saveSystemButton = new QPushButton("Save system to");
    loadSystemButton = new QPushButton("Load system from");

    buttonLayout->addWidget(updateSystemButton);
    buttonLayout->addWidget(saveSystemButton);
    buttonLayout->addWidget(loadSystemButton);

    mainLayout->addWidget(systemLabel);
    mainLayout->addWidget(systemTextEdit);
    mainLayout->addLayout(buttonLayout);

    connect(updateSystemButton, &QPushButton::clicked, this, &SystemCreationWidget::onUpdateSystem);
    connect(saveSystemButton, &QPushButton::clicked, this, &SystemCreationWidget::saveSystemTo);
    connect(loadSystemButton, &QPushButton::clicked, this, &SystemCreationWidget::loadSystem);
}

/**
 * @brief Gets the text currently in the system text editor.
 * @return System text as a std::string.
 */
std::string SystemCreationWidget::getSystemText() const
{
    return systemTextEdit->toPlainText().toStdString();
}

/**
 * @brief Sets the content of the system text editor.
 * @param text The new system text.
 */
void SystemCreationWidget::setSystemText(const std::string &text)
{
    systemTextEdit->setText(QString::fromStdString(text));
}

/**
 * @brief Parses the system text and updates the application system if valid.
 */
void SystemCreationWidget::onUpdateSystem()
{
    std::string text = getSystemText();

    try {
        auto system = SystemParserInterface::parseString(text);
        if (system.has_value()) {
            system->setSystemDefinitionText(text);
            Application::getInstance().setSystem(system.value());
            Q_EMIT systemUpdated();
        }
    } catch (const std::exception &e) {
        QMessageBox::critical(this, "Parsing error", e.what());
    }
}

/**
 * @brief Opens a dialog to save the current system to a file.
 */
void SystemCreationWidget::saveSystemTo()
{
    QFileDialog dialog(this);
    dialog.setFileMode(QFileDialog::AnyFile);
    QString filename = dialog.getSaveFileName(this, "Save file", "", "DQ System files (*.json *.dq)");
    auto system = Application::getInstance().getSystem();

    auto json = systemToJson(system);

    if (!filename.isEmpty()) {
        auto system = Application::getInstance().getSystem();
        auto json = systemToJson(system); // Convert system to nlohmann::json

        std::ofstream outFile(filename.toStdString());
        if (outFile.is_open()) {
            outFile << json.dump(4); // Pretty-print with indent
            outFile.close();
            QMessageBox::information(this, "Success", "File saved successfully.");
            Q_EMIT systemSaved();
        } else {
            QMessageBox::critical(this, "Error", "Could not open file for writing.");
        }
    }
}
/**
 * @brief Opens a dialog to load a system from a file, parses it, and updates the editor.
 */
void SystemCreationWidget::loadSystem()
{
    QFileDialog dialog(this);

    std::string filename = dialog.getOpenFileName(this, "Select file", " ", "DQ System files (*.json *.dq)").toStdString();
    if (filename.empty())
        return;

    std::ifstream inFile(filename);
    if (!inFile.is_open()) {
        QMessageBox::critical(this, "Error", "Could not open file for reading.");
        return;
    }

    nlohmann::json jsonData;
    try {
        inFile >> jsonData;
    } catch (const std::exception &e) {
        QMessageBox::critical(this, "Error", QString("Failed to parse system: ") + e.what());
        return;
    }

    std::string systemDefinition = jsonData["definition"];
    auto system = SystemParserInterface::parseString(systemDefinition); // This should return std::optional<System>
    if (system.has_value()) {
        Application::getInstance().setSystem(system.value());

        setSystemText(systemDefinition);

        system->setSystemDefinitionText(systemDefinition);
        for (auto &param : jsonData["parameters"]) {
            std::string name = param["name"];
            int n = param["n"];
            int N = param["n"];
            system->setObservableParameters(name, n, N);
        }
        Q_EMIT systemLoaded();
    } else {
        QMessageBox::warning(this, "Error", "System parsing failed. JSON not loaded.");
    }
}
