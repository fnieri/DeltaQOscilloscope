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

void NewPlotList::reset()
{
    this->blockSignals(true);
    while (count() != 0)
        delete takeItem(0);
    this->blockSignals(false);
    addItems();
}

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

std::vector<std::string> NewPlotList::getSelectedItems()
{
    std::vector<std::string> selectedItems;
    QList<QListWidgetItem *> selected = this->selectedItems();

    for (QListWidgetItem *item : selected) {
        selectedItems.push_back(item->text().toStdString());
    }
    return selectedItems;
}
