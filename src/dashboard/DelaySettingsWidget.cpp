/**
 * @file DelaySettingsWidget.cpp
 * @brief Implementation of the DelaySettingsWidget class.
 */

#include "DelaySettingsWidget.h"
#include "../Application.h"
#include <qlabel.h>
#include <qpushbutton.h>
#include <string>

DelaySettingsWidget::DelaySettingsWidget(QWidget *parent)
    : QWidget(parent)
{
    mainLayout = new QVBoxLayout(this);
    settingsLayout = new QHBoxLayout(this);

    settingsLabel = new QLabel("Set the parameters for a probe.");
    mainLayout->addWidget(settingsLabel);

    observableComboBox = new QComboBox();
    settingsLayout->addWidget(observableComboBox);

    delaySlider = new QSlider(Qt::Horizontal);
    delaySlider->setRange(-10, 10);
    delaySlider->setTickInterval(1);
    delaySlider->setTickPosition(QSlider::TicksBelow);
    settingsLayout->addWidget(delaySlider);

    binSpinBox = new QSpinBox();
    binSpinBox->setRange(1, 1000);
    binSpinBox->setValue(10);
    settingsLayout->addWidget(binSpinBox);

    mainLayout->addLayout(settingsLayout);

    maxDelayLabel = new QLabel("Bin width: \nMax delay: ");
    mainLayout->addWidget(maxDelayLabel);

    saveDelayButton = new QPushButton("Save delay");
    mainLayout->addWidget(saveDelayButton);

    connect(delaySlider, &QSlider::valueChanged, this, &DelaySettingsWidget::updateMaxDelay);
    connect(binSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, &DelaySettingsWidget::updateMaxDelay);
    connect(saveDelayButton, &QPushButton::clicked, this, &DelaySettingsWidget::onSaveDelayClicked);
    connect(observableComboBox, &QComboBox::currentTextChanged, this, &DelaySettingsWidget::loadObservableSettings);

    Application::getInstance().addObserver([this]() { this->populateComboBox(); });
}

/**
 * @brief Populates the combo box with available observables from the system.
 */
void DelaySettingsWidget::populateComboBox()
{
    auto system = Application::getInstance().getSystem();
    if (!system)
        return;

    observableComboBox->clear();
    for (const auto &[name, obs] : system->getObservables()) {
        if (obs)
            observableComboBox->addItem(QString::fromStdString(name));
    }
}

/**
 * @brief Loads delay settings for the currently selected observable.
 */
void DelaySettingsWidget::loadObservableSettings()
{
    auto system = Application::getInstance().getSystem();
    if (!system)
        return;

    QString observableName = observableComboBox->currentText();
    if (observableName.isEmpty())
        return;

    auto observable = system->getObservable(observableName.toStdString());

    auto exponent = observable->getDeltaTExp();
    auto bins = observable->getNBins();
    delaySlider->setValue(exponent);
    binSpinBox->setValue(bins);

    updateMaxDelay();
}

/**
 * @brief Computes the current maximum delay.
 * @return Maximum delay value based on bin count and delay exponent.
 */
double DelaySettingsWidget::getMaxDelayMs() const
{
    int exponent = delaySlider->value();
    int bins = binSpinBox->value();
    return 1.0 * std::pow(2.0, exponent) * bins;
}

/**
 * @brief Updates the label that shows the computed max delay value.
 */
void DelaySettingsWidget::updateMaxDelay()
{
    double delay = getMaxDelayMs();
    std::string labelString = "Bin width: " + std::to_string((1 * std::pow(2.0, delaySlider->value()))) + "ms \n";
    labelString += "Max delay is: " + std::to_string(delay) + "ms";
    maxDelayLabel->setText(QString::fromStdString(labelString));
}

/**
 * @brief Saves the current delay settings to the system and emits a change signal.
 */
void DelaySettingsWidget::onSaveDelayClicked()
{
    auto system = Application::getInstance().getSystem();
    if (!system)
        return;

    QString name = observableComboBox->currentText();
    if (name.isEmpty())
        return;

    int exponent = delaySlider->value();
    int bins = binSpinBox->value();
    std::string nameString = name.toStdString();
    system->setObservableParameters(nameString, exponent, bins);

    Q_EMIT delayParametersChanged();
}
