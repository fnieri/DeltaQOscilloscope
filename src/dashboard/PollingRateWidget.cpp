#include "PollingRateWidget.h"

PollingRateWidget::PollingRateWidget(QWidget *parent)
    : QWidget(parent)
{
    pollingRates = {100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 5000, 10000};

    slider = new QSlider(Qt::Horizontal);
    slider->setMinimum(0);
    slider->setMaximum(pollingRates.size() - 1);
    slider->setValue(0);

    slider->setTickInterval(1);
    slider->setTickPosition(QSlider::TicksBelow);

    valueLabel = new QLabel(QString("Polling Rate: %1 ms").arg(pollingRates[0]));

    saveButton = new QPushButton("Save polling rate");

    auto *layout = new QVBoxLayout();
    layout->addWidget(slider);
    layout->addWidget(valueLabel);
    layout->addWidget(saveButton);
    setLayout(layout);

    connect(slider, &QSlider::valueChanged, this, &PollingRateWidget::onSliderValueChanged);
    connect(saveButton, &QPushButton::clicked, this, &PollingRateWidget::onSaveClicked);
}

void PollingRateWidget::onSliderValueChanged(int value)
{
    int ms = pollingRates[value];
    valueLabel->setText(QString("Polling Rate: %1 ms").arg(ms));
}

void PollingRateWidget::onSaveClicked()
{
    int ms = pollingRates[slider->value()];
    Q_EMIT onPollingRateChanged(ms);
}
