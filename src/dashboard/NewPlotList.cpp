/**
 * @file NewPlotList.cpp
 * @brief Implementation of the NewPlotList class, which provides a UI list for selecting observables to create a new plot.
 */

#include "NewPlotList.h"
#include "../Application.h"
#include <iostream>
#include <qlistwidget.h>

NewPlotList::NewPlotList(QWidget *parent)
    : QListWidget(parent)
{
    setSelectionMode(QAbstractItemView::MultiSelection);
    Application::getInstance().addObserver([this]() { this->reset(); });
}

/**
 * @brief Clears and repopulates the list with updated observables.
 *
 */
void NewPlotList::reset()
{
    this->blockSignals(true);
    while (count() != 0)
        delete takeItem(0);
    this->blockSignals(false);
    addItems();
}

/**
 * @brief Adds all observables from the system as items in the list.
 */
void NewPlotList::addItems()
{
    auto system = Application::getInstance().getSystem();
    auto observables = system->getObservables();

    for (const auto &obs : observables) {
        if (obs.second) {
            QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(obs.first), this);
            item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
        }
    }
}

/**
 * @brief Returns a list of selected observable names.
 * @return A vector of strings corresponding to selected item labels.
 */
std::vector<std::string> NewPlotList::getSelectedItems()
{
    std::vector<std::string> selectedItems;
    QList<QListWidgetItem *> selected = this->selectedItems();

    for (QListWidgetItem *item : selected) {
        selectedItems.push_back(item->text().toStdString());
    }
    return selectedItems;
}

/**
 * @brief Deselects all currently selected items in the list.
 */
void NewPlotList::deselectAll()
{
    clearSelection();
}
