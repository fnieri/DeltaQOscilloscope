#pragma once

#include <QLabel>
#include <QPushButton>
#include <QSlider>
#include <QVBoxLayout>
#include <QWidget>

/**
 * @class SamplingRateWidget
 * @brief A widget for saving sampling  rate intervals.
 *
 * Provides a slider interface to select from predefined sampling rates
 * (in milliseconds) and emits the selected rate when saved.
 */
class SamplingRateWidget : public QWidget
{
    Q_OBJECT

public:
    explicit SamplingRateWidget(QWidget *parent = nullptr);

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
     * @brief Emitted when a new sampling rate is saved.
     * @param milliseconds The selected sampling rate in milliseconds.
     */
    void onSamplingRateChanged(int milliseconds);

private:
    QSlider *slider; ///< Slider for selecting sampling rate
    QLabel *valueLabel; ///< Displays the current sampling rate
    QPushButton *saveButton; ///< Button to save the selected rate
    QVector<int> samplingRates; ///< Available sampling rate options (ms)
};
