
#include "ComponentEditDialog.h"
#include <QHBoxLayout>
#include <QLabel>
#include <QMessageBox>
#include <QVBoxLayout>
ComponentEditDialog::ComponentEditDialog(QWidget *parent)
    : QDialog(parent)
{
    QVBoxLayout *mainLayout = new QVBoxLayout(this);

    nameLineEdit = new QLineEdit;
    nameLineEdit->setPlaceholderText("Name");

    startEventLineEdit = new QLineEdit;
    startEventLineEdit->setPlaceholderText("Start Event");

    endEventLineEdit = new QLineEdit;
    endEventLineEdit->setPlaceholderText("End Event");

    operatorChoice = new QComboBox;
    operatorChoice->addItems({"Outcome", "First-to-finish", "All-to-finish", "Probabilistic choice"});

    // Buttons
    confirmButton = new QPushButton("Confirm");
    cancelButton = new QPushButton("Cancel");

    // Layouts
    mainLayout->addWidget(new QLabel("Name:"));
    mainLayout->addWidget(nameLineEdit);
    mainLayout->addWidget(new QLabel("Start Event:"));
    mainLayout->addWidget(startEventLineEdit);
    mainLayout->addWidget(new QLabel("End Event:"));
    mainLayout->addWidget(endEventLineEdit);
    mainLayout->addWidget(new QLabel("Operator Type:"));
    mainLayout->addWidget(operatorChoice);

    QHBoxLayout *buttonLayout = new QHBoxLayout;
    buttonLayout->addWidget(confirmButton);
    buttonLayout->addWidget(cancelButton);

    mainLayout->addLayout(buttonLayout);

    // Connect buttons
    connect(confirmButton, &QPushButton::clicked, this, &ComponentEditDialog::onConfirm);
    connect(cancelButton, &QPushButton::clicked, this, &QDialog::reject);
}

void ComponentEditDialog::setFields(const JsonComponent &component)
{
    nameLineEdit->setText(QString::fromStdString(component.name));
    startEventLineEdit->setText(QString::fromStdString(component.startEvent));
    endEventLineEdit->setText(QString::fromStdString(component.endEvent));

    if (component.type == "O")
        operatorChoice->setCurrentIndex(0);
    else if (component.type == "F")
        operatorChoice->setCurrentIndex(1);
    else if (component.type == "A")
        operatorChoice->setCurrentIndex(2);
    else if (component.type == "P")
        operatorChoice->setCurrentIndex(3);
}

JsonComponent ComponentEditDialog::getEditedComponent() const
{
    std::string operatorShortCode;
    QString operatorType = operatorChoice->currentText();

    if (operatorType == "Outcome")
        operatorShortCode = "O";
    else if (operatorType == "First-to-finish")
        operatorShortCode = "F";
    else if (operatorType == "Probabilistic choice")
        operatorShortCode = "P";
    else if (operatorType == "All-to-finish")
        operatorShortCode = "A";

    return JsonComponent {
        nameLineEdit->text().toStdString(), startEventLineEdit->text().toStdString(), endEventLineEdit->text().toStdString(), operatorShortCode};
}

void ComponentEditDialog::onConfirm()
{
    if (nameLineEdit->text().trimmed().isEmpty() || startEventLineEdit->text().trimmed().isEmpty()) {
        QMessageBox::warning(this, "Validation Error", "Name and Start Event must not be empty.");
        return;
    }

    accept();
}
