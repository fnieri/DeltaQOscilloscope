#pragma once

#include <QCheckBox>
#include <QComboBox>
#include <QFormLayout>
#include <QListWidget>
#include <QMap>
#include <QSpinBox>
#include <QVBoxLayout>
#include <QWidget>

#include "../Application.h"
#include "src/maths/TriggerManager.h"

/**
 * @class TriggersTab
 * @brief Widget for managing and monitoring trigger conditions on observables.
 *
 * Provides UI for:
 * - Setting up trigger conditions (sample limits, QTA violations)
 * - Displaying triggered events
 * - Viewing snapshots of triggered states
 */
class TriggersTab : public QWidget
{
    Q_OBJECT

public:
    explicit TriggersTab(QWidget *parent = nullptr);

    ~TriggersTab();

    /**
     * @brief Adds a triggered message to the display list.
     * @param msg The message to display.
     */
    void addTriggeredMessage(const QString &msg);

private Q_SLOTS:
    /**
     * @brief Handles observable selection changes.
     * @param name The newly selected observable name.
     */
    void onObservableChanged(const QString &name);

    /**
     * @brief Handles trigger condition changes.
     */
    void onTriggerChanged();

    /**
     * @brief Handles triggered item clicks to show snapshots.
     * @param item The clicked list item.
     */
    void onTriggeredItemClicked(QListWidgetItem *item);

private:
    /**
     * @brief Captures snapshots when triggers are activated.
     * @param time The timestamp of the trigger event.
     * @param name The name of the observable that triggered.
     */
    void captureSnapshots(std::uint64_t time, const std::string &name);

    /**
     * @brief Populates the observable dropdown list.
     */
    void populateObservables();

    /**
     * @brief Updates checkbox states based on current triggers.
     */
    void updateCheckboxStates();

    QVBoxLayout *mainLayout;             ///< Main vertical layout
    QFormLayout *formLayout;             ///< Form layout for controls
    QComboBox *observableComboBox;       ///< Dropdown for observable selection

    QWidget *sampleLimitWidget;          ///< Container for sample limit controls
    QHBoxLayout *sampleLimitLayout;      ///< Layout for sample limit controls
    QCheckBox *sampleLimitCheckBox;      ///< Checkbox to enable sample limit trigger
    QSpinBox *sampleLimitSpinBox;        ///< Spinbox for sample limit threshold

    QCheckBox *qtaBoundsCheckBox;        ///< Checkbox for QTA bounds violation trigger

    QListWidget *triggeredList;          ///< List widget for triggered events

    /**
     * @brief Gets the currently selected observable.
     * @return Shared pointer to the current observable.
     * @throws std::runtime_error if system or observable doesn't exist.
     */
    std::shared_ptr<Observable> getCurrentObservable();

    static constexpr int sampleLimitThreshold = 500;      ///< Default sample limit threshold
    static constexpr double failureRateThreshold = 0.95;  ///< Failure rate threshold constant
};
