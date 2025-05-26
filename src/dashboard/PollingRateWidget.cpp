#include "PollingRateWidget.h"

PollingRateWidget::PollingRateWidget(QWidget *parent)
    : QWidget(parent)
{
    // Available polling rate options in milliseconds
    pollingRates = {100, 200, 300, 400, 500, 600, 700, 800, 900,
                   1000, 2000, 5000, 10000};

    // Configure rate selection slider
    slider = new QSlider(Qt::Horizontal);
    slider->setMinimum(0);
    slider->setMaximum(pollingRates.size() - 1);
    slider->setValue(0);  // Default to first option (100ms)
    slider->setTickInterval(1);
    slider->setTickPosition(QSlider::TicksBelow);

    // Initialize display label with default value
    valueLabel = new QLabel(QString("Polling Rate: %1 ms").arg(pollingRates[0]));

    // Initialize save button
    saveButton = new QPushButton("Save polling rate");

    // Set up layout
    auto *layout = new QVBoxLayout();
    layout->addWidget(slider);
    layout->addWidget(valueLabel);
    layout->addWidget(saveButton);
    setLayout(layout);

    // Connect signals
    connect(slider, &QSlider::valueChanged,
            this, &PollingRateWidget::onSliderValueChanged);
    connect(saveButton, &QPushButton::clicked,
            this, &PollingRateWidget::onSaveClicked);
}

/**
 * @brief Updates the displayed rate when slider changes.
 * @param value The current slider index (0-based).
 *
 * Converts slider index to actual milliseconds and updates the display label.
 */
void PollingRateWidget::onSliderValueChanged(int value)
{
    int ms = pollingRates[value];
    valueLabel->setText(QString("Polling Rate: %1 ms").arg(ms));
}

/**
 * @brief Emits the selected polling rate when save is clicked.
 *
 * Gets the current slider value, converts it to milliseconds,
 * and emits the onPollingRateChanged signal.
 */
void PollingRateWidget::onSaveClicked()
{
    int ms = pollingRates[slider->value()];
    Q_EMIT onPollingRateChanged(ms);
}
