#include "SystemCreationWidget.h"
#include "../Application.h"
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
            Application::getInstance().setSystem(system.value());
            system->setSystemDefinitionText(text);
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
    QString filename = dialog.getSaveFileName(this, "Save file", "", "All files (* *.dq)");

    if (!filename.isEmpty()) {
        std::string systemText = getSystemText();
        auto system = SystemParserInterface::parseString(systemText);

        if (system.has_value()) {
            std::ofstream outFile(filename.toStdString());
            if (outFile.is_open()) {
                outFile << systemText;
                outFile.close();
                QMessageBox::information(this, "Success", "File saved successfully.");
                Q_EMIT systemSaved();
            } else {
                QMessageBox::critical(this, "Error", "Could not open file for writing.");
            }
        } else {
            QMessageBox::warning(this, "Error", "System parsing failed. File not saved.");
        }
    }
}

/**
 * @brief Opens a dialog to load a system from a file, parses it, and updates the editor.
 */
void SystemCreationWidget::loadSystem()
{
    QFileDialog dialog(this);
    std::string filename = dialog.getOpenFileName(this, "Select file", " ", "All files (* *.dq)").toStdString();

    auto system = SystemParserInterface::parseFile(filename);
    if (system.has_value()) {
        Application::getInstance().setSystem(system.value());
        std::ifstream file(filename);
        std::string str;
        std::string fileContents;
        while (std::getline(file, str)) {
            fileContents += str;
            fileContents.push_back('\n');
        }

        system->setSystemDefinitionText(fileContents);
        setSystemText(fileContents);
        Q_EMIT systemLoaded();
    }
}
