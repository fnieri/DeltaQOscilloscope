#pragma once

#include <QLabel>
#include <QPushButton>
#include <QSlider>
#include <QVBoxLayout>
#include <QWidget>

/**
 * @class PollingRateWidget
 * @brief A widget for selecting and saving polling rate intervals.
 *
 * Provides a slider interface to select from predefined polling rates
 * (in milliseconds) and emits the selected rate when saved.
 */
class PollingRateWidget : public QWidget
{
    Q_OBJECT

public:
    explicit PollingRateWidget(QWidget *parent = nullptr);

private Q_SLOTS:
    /**
     * @brief Handles slider value changes to update the displayed rate.
     * @param value The current slider index (0-based).
     */
    void onSliderValueChanged(int value);

    /**
     * @brief Handles save button clicks to emit the selected rate.
     */
    void onSaveClicked();

Q_SIGNALS:
    /**
     * @brief Emitted when a new polling rate is saved.
     * @param milliseconds The selected polling rate in milliseconds.
     */
    void onPollingRateChanged(int milliseconds);

private:
    QSlider *slider;            ///< Slider for selecting polling rate
    QLabel *valueLabel;         ///< Displays the current polling rate
    QPushButton *saveButton;    ///< Button to save the selected rate
    QVector<int> pollingRates;  ///< Available polling rate options (ms)
};
