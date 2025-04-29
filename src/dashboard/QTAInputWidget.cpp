#include "QTAInputWidget.h"
#include "../Application.h"
#include <QDoubleValidator>
#include <QMessageBox>
#include <QPushButton>

QTAInputWidget::QTAInputWidget(QWidget *parent)
    : QWidget(parent)
{
    auto layout = new QFormLayout(this);


    qtaLabel = new QLabel(this);
    qtaLabel->setText("Set QTA for an observable");
    layout->addRow(qtaLabel);
    observableComboBox = new QComboBox(this);
    layout->addRow("Observable:", observableComboBox);


    perc25Edit = new QLineEdit(this);
    perc25Edit->setPlaceholderText("Seconds (s)");

    perc50Edit = new QLineEdit(this);
    perc50Edit->setPlaceholderText("Seconds (s)");

    perc75Edit = new QLineEdit(this);
    perc75Edit->setPlaceholderText("Seconds (s)");

    cdfMaxEdit = new QLineEdit(this);
    cdfMaxEdit->setPlaceholderText("Value between 0 and 1");

    layout->addRow("25th Percentile (s):", perc25Edit);
    layout->addRow("50th Percentile (s):", perc50Edit);
    layout->addRow("75th Percentile (s):", perc75Edit);
    layout->addRow("Max. allowed failure (0-1):", cdfMaxEdit);

    connect(observableComboBox, &QComboBox::currentTextChanged, this, &QTAInputWidget::loadObservableSettings);

    Application::getInstance().addObserver([this]() { this->populateComboBox(); });
    saveButton = new QPushButton("Save QTA Settings", this);
    layout->addRow(saveButton);
    connect(saveButton, &QPushButton::clicked, this, &QTAInputWidget::onSaveButtonClicked);

    setLayout(layout);


    Application::getInstance().addObserver([this]() { this->populateComboBox(); });
}

void QTAInputWidget::populateComboBox()
{
    auto system = Application::getInstance().getSystem();
    if (!system)
        return;

    observableComboBox->clear();
    for (const auto &[name, _] : system->getProbes()) {
        observableComboBox->addItem(QString::fromStdString(name));
    }
    for (const auto &[name, _] : system->getOutcomes()) {
        observableComboBox->addItem(QString::fromStdString(name));
    }
}

void QTAInputWidget::loadObservableSettings()
{
    auto system = Application::getInstance().getSystem();
    if (!system)
        return;
    std::string observableName = observableComboBox->currentText().toStdString();
    auto observable = system->getObservable(observableName);
    if (observable) {
        auto qta = observable->getQTA();
        perc25Edit->setText(QString::number(qta.perc_25, 'f', 6));  // 6 decimal places
        perc50Edit->setText(QString::number(qta.perc_50, 'f', 6));
        perc75Edit->setText(QString::number(qta.perc_75, 'f', 6));
        cdfMaxEdit->setText(QString::number(qta.cdfMax, 'f', 6));
    }
}

void QTAInputWidget::onSaveButtonClicked()
{
    auto system = Application::getInstance().getSystem();
    if (!system)
        return;

    std::string observableName = observableComboBox->currentText().toStdString();
    auto observable = system->getObservable(observableName);
    if (!observable)
        return;

    try {
        QTA newQTA = QTA::create(getPerc25(), getPerc50(), getPerc75(), getCdfMax());
        observable->setQTA(newQTA);
    }
    catch (std::exception &e) {
        QMessageBox::warning(this, "Error", e.what());
    }
}


double QTAInputWidget::getPerc25() const { return perc25Edit->text().toDouble(); }
double QTAInputWidget::getPerc50() const { return perc50Edit->text().toDouble(); }
double QTAInputWidget::getPerc75() const { return perc75Edit->text().toDouble(); }
double QTAInputWidget::getCdfMax() const { return cdfMaxEdit->text().toDouble(); }

QString QTAInputWidget::getSelectedObservable() const { return observableComboBox->currentText(); }
