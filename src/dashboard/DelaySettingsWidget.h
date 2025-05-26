#pragma once

#include <QComboBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSlider>
#include <QSpinBox>
#include <QVBoxLayout>
#include <QWidget>
#include <cmath>

/**
 * @brief A widget for configuring delay parameters for a selected observable.
 */
class DelaySettingsWidget : public QWidget
{
    Q_OBJECT

public:
    explicit DelaySettingsWidget(QWidget *parent = nullptr);

    /**
     * @brief Populates the observable combo box using the system's observable list.
     */
    void populateComboBox();

    /**
     * @brief Computes the maximum delay in milliseconds based on the slider and spinbox values.
     * @return Maximum delay in milliseconds.
     */
    double getMaxDelayMs() const;

Q_SIGNALS:
    /**
     * @brief Signal emitted when delay parameters have been changed and saved.
     */
    void delayParametersChanged();

private Q_SLOTS:
    /**
     * @brief Updates the label showing the current maximum delay based on UI values.
     */
    void updateMaxDelay();

    /**
     * @brief Handles saving the currently selected delay parameters to the system.
     */
    void onSaveDelayClicked();

    /**
     * @brief Loads the saved settings for the currently selected observable.
     */
    void loadObservableSettings();

private:
    QVBoxLayout *mainLayout;           ///< Main layout container.
    QLabel *settingsLabel;            ///< Label describing the purpose of the widget.
    QLabel *maxDelayLabel;            ///< Label showing the computed max delay.

    QHBoxLayout *settingsLayout;      ///< Layout for parameter controls.
    QComboBox *observableComboBox;    ///< Combo box for selecting observables.
    QSlider *delaySlider;             ///< Slider to set delay exponent.
    QSpinBox *binSpinBox;             ///< Spin box to set number of bins.

    QPushButton *saveDelayButton;     ///< Button to save the delay configuration.
};
