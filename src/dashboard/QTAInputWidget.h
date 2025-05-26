#ifndef QTAINPUTWIDGET_H
#define QTAINPUTWIDGET_H

#include <QWidget>
#include <QLineEdit>
#include <QComboBox>
#include <QFormLayout>
#include <QLabel>
#include <QPushButton>

/**
 * @class QTAInputWidget
 * @brief A Qt widget for configuring QTAs for observables.
 */
class QTAInputWidget : public QWidget
{
    Q_OBJECT

public:
    explicit QTAInputWidget(QWidget *parent = nullptr);

    /**
     * @brief Gets the 25th percentile value (in seconds).
     * @return The value entered in the 25th percentile field.
     */
    double getPerc25() const;

    /**
     * @brief Gets the 50th percentile (median) value (in seconds).
     * @return The value entered in the 50th percentile field.
     */
    double getPerc50() const;

    /**
     * @brief Gets the 75th percentile value (in seconds).
     * @return The value entered in the 75th percentile field.
     */
    double getPerc75() const;

    /**
     * @brief Gets the maximum allowed CDF value (0 to 1).
     * @return The value entered in the CDF max field.
     */
    double getCdfMax() const;

    /**
     * @brief Gets the currently selected observable name.
     * @return The name of the selected observable.
     */
    QString getSelectedObservable() const;

public Q_SLOTS:
    /**
     * @brief Populates the observable dropdown with available observables.
     * @note Called automatically when the system updates.
     */
    void populateComboBox();

    /**
     * @brief Loads QTA settings for the selected observable into the UI fields.
     * @note Triggered when the dropdown selection changes.
     */
    void loadObservableSettings();

    /**
     * @brief Saves the current QTA settings to the system.
     * @note Called when the "Save" button is clicked.
     * @throws std::exception if validation fails (e.g., invalid CDF value).
     */
    void onSaveButtonClicked();

private:
    QComboBox *observableComboBox;   ///< Dropdown to select an observable (probe/outcome).
    QLineEdit *perc25Edit;           ///< Input field for the 25th percentile (seconds).
    QLineEdit *perc50Edit;           ///< Input field for the 50th percentile (seconds).
    QLineEdit *perc75Edit;           ///< Input field for the 75th percentile (seconds).
    QLineEdit *cdfMaxEdit;           ///< Input field for the max CDF value (0-1).
    QPushButton *saveButton;         ///< Button to save QTA settings.
    QLabel *qtaLabel;                ///< Label describing the widget's purpose.
};

#endif // QTAINPUTWIDGET_H
