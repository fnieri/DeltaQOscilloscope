#pragma once

#ifndef DQ_PLOT_LIST_H
#define DQ_PLOT_LIST_H

#include "DQPlotController.h"
#include <QCheckBox>
#include <QListWidget>
#include <QPushButton>
#include <QVBoxLayout>
#include <QWidget>

class DQPlotController;

/**
 * @class DQPlotList
 * @brief A widget that displays and manages lists of available and selected plot components for a selected plot.
 *
 */
class DQPlotList : public QWidget
{
    Q_OBJECT

public:
    explicit DQPlotList(DQPlotController *controller, QWidget *parent = nullptr);

    /**
     * @brief Resets the widget's state.
     */
    void reset();

    /**
     * @brief Checks if the widget is empty after reset.
     * @return true if no components are selected, false otherwise.
     */
    bool isEmptyAfterReset();

    /**
     * @brief Updates both the available and selected lists.
     */
    void updateLists();

    /**
     * @brief Default destructor.
     */
    ~DQPlotList() = default;

private Q_SLOTS:
    /**
     * @brief Handles confirmation of selected components to add.
     */
    void onConfirmSelection();

    /**
     * @brief Handles removal of selected components.
     */
    void onRemoveSelection();

private:
    /**
     * @brief Adds an item to either the available or selected list.
     * @param name The name of the component.
     * @param isSelected Whether the item should be in the selected list.
     * @param category The category for the item (used for grouping).
     */
    void addItemToList(const std::string &name, bool isSelected, const QString &category);

    DQPlotController *controller; ///< The controller managing plot components.

    QListWidget *selectedList;    ///< List widget showing currently selected components.
    QListWidget *availableList;   ///< List widget showing available components.

    QPushButton *addButton;       ///< Button to add selected components.
    QPushButton *removeButton;    ///< Button to remove selected components.
};

#endif // DQ_PLOT_LIST_H
