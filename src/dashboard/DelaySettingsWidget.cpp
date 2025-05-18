
#include "DelaySettingsWidget.h"
#include "../Application.h"
#include <qlabel.h>
#include <qpushbutton.h>
DelaySettingsWidget::DelaySettingsWidget(QWidget *parent)
    : QWidget(parent)
{
    mainLayout = new QVBoxLayout(this);
    settingsLayout = new QHBoxLayout(this);

    settingsLabel = new QLabel("Set the parameters for an observable");
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

    maxDelayLabel = new QLabel("Max delay is: ");
    mainLayout->addWidget(maxDelayLabel);
    saveDelayButton = new QPushButton("Save delay");
    mainLayout->addWidget(saveDelayButton);
    connect(delaySlider, &QSlider::valueChanged, this, &DelaySettingsWidget::updateMaxDelay);
    connect(binSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, &DelaySettingsWidget::updateMaxDelay);
    connect(saveDelayButton, &QPushButton::clicked, this, &DelaySettingsWidget::onSaveDelayClicked);
    connect(observableComboBox, &QComboBox::currentTextChanged, this, &DelaySettingsWidget::loadObservableSettings);

    Application::getInstance().addObserver([this]() { this->populateComboBox(); });
}

void DelaySettingsWidget::populateComboBox()
{
    auto system = Application::getInstance().getSystem();
    if (!system)
        return;
    observableComboBox->clear();
    for (const auto [name, _] : system->getProbes()) {
        observableComboBox->addItem(QString::fromStdString(name));
    }
    for (const auto &[name, _] : system->getOutcomes()) {
        observableComboBox->addItem(QString::fromStdString(name));
    }
}

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

double DelaySettingsWidget::getMaxDelayMs() const
{
    int exponent = delaySlider->value();
    int bins = binSpinBox->value();
    return 1.0 * std::pow(2.0, exponent) * bins;
}

void DelaySettingsWidget::updateMaxDelay()
{
    double delay = getMaxDelayMs();
    maxDelayLabel->setText(QString("Max delay is: %1 ms").arg(delay, 0, 'f', 2));
}

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
