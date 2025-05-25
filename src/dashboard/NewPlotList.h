#pragma once

#ifndef NEW_PLOT_LIST_H
#define NEW_PLOT_LIST_H

#include "../diagram/System.h"
#include <QCheckBox>
#include <QListWidget>
#include <qlistwidget.h>
#include <qwidget.h>

/**
 * @brief A list widget for selecting observables to create a new plot.
 *
 */
class NewPlotList : public QListWidget
{
    Q_OBJECT

public:
    explicit NewPlotList(QWidget *parent = nullptr);

    /**
     * @brief Gets the names of all currently selected observables.
     * @return A vector of strings representing selected observables.
     */
    std::vector<std::string> getSelectedItems();

    /**
     * @brief Deselects all currently selected items in the list.
     */
    void deselectAll();

    /**
     * @brief Clears the list and repopulates it with updated observables from the system.
     */
    void reset();

private:
    /**
     * @brief Adds observable items to the list from the current system.
     */
    void addItems();
};

#endif // NEW_PLOT_LIST_H
