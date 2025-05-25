#ifndef SIDEBAR_H
#define SIDEBAR_H

#include "DQPlotList.h"
#include "NewPlotList.h"
#include "PollingRateWidget.h"
#include "SystemCreationWidget.h"
#include <QComboBox>
#include <QLabel>
#include <QPushButton>
#include <QSpinBox>
#include <QSplitter>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QWidget>
#include <qboxlayout.h>

/**
 * @class Sidebar
 * @brief Main sidebar widget containing plot management controls and system configuration.
 *
 * The Sidebar provides UI components for:
 * - Creating new plots
 * - Managing existing plots
 * - System configuration
 * - Polling rate adjustment
 */
class Sidebar : public QWidget
{
    Q_OBJECT

    QVBoxLayout *newPlotListLayout;      ///< Layout for new plot selection components
    QWidget *newPlotListWidget;          ///< Container widget for new plot controls
    QLabel *newPlotLabel;                ///< Label for new plot section
    NewPlotList *newPlotList;            ///< List widget for selecting probes for new plots
    QPushButton *addNewPlotButton;       ///< Button to create new plot

    QWidget *currentPlotWidget;          ///< Container widget for current plot controls
    QVBoxLayout *currentPlotLayout;      ///< Layout for current plot components
    QLabel *currentPlotLabel;            ///< Label for current plot section
    DQPlotList *currentPlotList = nullptr; ///< List widget for managing current plot's probes

    QSplitter *mainSplitter;             ///< Main splitter organizing sections vertically
    QVBoxLayout *layout;                 ///< Main layout of the sidebar

    SystemCreationWidget *systemCreationWidget; ///< Widget for system creation/configuration
    PollingRateWidget *pollingRateWidget; ///< Widget for adjusting polling rate

Q_SIGNALS:
    /**
     * @brief Emitted when the "Add plot" button is clicked.
     */
    void addPlotClicked();

    /**
     * @brief Emitted when polling rate is changed.
     * @param milliseconds The new polling rate in milliseconds.
     */
    void onPollingRateChanged(int milliseconds);

private Q_SLOTS:
    /**
     * @brief Handles "Add plot" button click event.
     */
    void onAddPlotClicked();

    /**
     * @brief Handles polling rate change events.
     * @param ms The new polling rate in milliseconds.
     */
    void handlePollingRateChanged(int ms);

public:
    /**
     * @brief Constructs a Sidebar widget.
     * @param parent The parent widget (optional).
     */
    explicit Sidebar(QWidget *parent = nullptr);

    /**
     * @brief Sets the current plot list widget.
     * @param currentPlotList The DQPlotList widget to display.
     */
    void setCurrentPlotList(DQPlotList *currentPlotList);

    /**
     * @brief Hides the current plot management section.
     */
    void hideCurrentPlot();

    /**
     * @brief Gets the new plot list widget.
     * @return Pointer to the NewPlotList widget.
     */
    NewPlotList *getPlotList() const
    {
        return newPlotList;
    }

    /**
     * @brief Clears new plot selection after plot creation.
     */
    void clearOnAdd();
};

#endif
