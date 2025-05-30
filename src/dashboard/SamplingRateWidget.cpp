#include "SamplingRateWidget.h"

SamplingRateWidget::SamplingRateWidget(QWidget *parent)
    : QWidget(parent)
{
    // Available sampling rate options in milliseconds
    samplingRates = {100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 2000, 5000, 10000};

    // Configure rate selection slider
    slider = new QSlider(Qt::Horizontal);
    slider->setMinimum(0);
    slider->setMaximum(samplingRates.size() - 1);
    slider->setValue(1);
    slider->setTickInterval(1);
    slider->setTickPosition(QSlider::TicksBelow);

    valueLabel = new QLabel(QString("Sampling Rate: %1 ms").arg(samplingRates[0]));

    saveButton = new QPushButton("Save sampling rate");

    // Set up layout
    auto *layout = new QVBoxLayout();
    layout->addWidget(slider);
    layout->addWidget(valueLabel);
    layout->addWidget(saveButton);
    setLayout(layout);

    // Connect signals
    connect(slider, &QSlider::valueChanged, this, &SamplingRateWidget::onSliderValueChanged);
    connect(saveButton, &QPushButton::clicked, this, &SamplingRateWidget::onSaveClicked);
}

/**
 * @brief Updates the displayed rate when slider changes.
 * @param value The current slider index (0-based).
 *
 * Converts slider index to actual milliseconds and updates the display label.
 */
void SamplingRateWidget::onSliderValueChanged(int value)
{
    int ms = samplingRates[value];
    valueLabel->setText(QString("Sampling Rate: %1 ms").arg(ms));
}

/**
 * @brief Emits the selected sampling rate when save is clicked.
 *
 * Gets the current slider value, converts it to milliseconds,
 * and emits the onSamplingRateChanged signal.
 */
void SamplingRateWidget::onSaveClicked()
{
    int ms = samplingRates[slider->value()];
    Q_EMIT onSamplingRateChanged(ms);
}
