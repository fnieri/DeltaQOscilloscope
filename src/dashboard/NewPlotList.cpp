#include "NewPlotList.h"
#include <iostream>
#include <qlistwidget.h>
using Outcomes = std::unordered_map<std::string, std::shared_ptr<Outcome>>;
using Operators = std::unordered_map<std::string, std::shared_ptr<Operator>>;
using Probes = std::unordered_map<std::string, std::shared_ptr<Probe>>;

NewPlotList::NewPlotList(std::shared_ptr<System> system, QWidget *parent)
    : system {system}
    , QListWidget(parent)
{
    setSelectionMode(QAbstractItemView::MultiSelection);
    create();
}

void NewPlotList::create()
{
    addItems();
}

void NewPlotList::addCategory(const QString &category)
{
    assert(!systemItems.contains(category));

    QListWidgetItem *categoryItem = new QListWidgetItem(category, this);
    categoryItem->setFlags(Qt::NoItemFlags); // Non-selectable label
    categoryItem->setFont(QFont("Arial", 10, QFont::Bold)); // Bold label

    systemItems.insert(category, categoryItem);
}

void NewPlotList::addItems()
{
    addProbes();
    addOutcomes();
    addOperators();
}

void NewPlotList::addProbes()
{
    QString category = "Probes:";
    addCategory(category);

    Probes probes = system->getProbes();
    for (const auto &probe : probes) {
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(probe.first), this);
        item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
    }
}

void NewPlotList::addOutcomes()
{
    QString category = "Outcomes:";
    addCategory(category);

    Outcomes outcomes = system->getOutcomes();
    for (const auto &outcome : outcomes) {
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(outcome.first), this);
        item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
    }
}

void NewPlotList::addOperators()
{
    QString category = "Operators:";
    addCategory(category);

    Operators operators = system->getOperators();
    for (const auto &op : operators) {
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(op.first), this);
        item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
    }
}

std::vector<std::string> NewPlotList::getSelectedItems()
{
    std::vector<std::string> selectedItems;
    QList<QListWidgetItem *> selected = this->selectedItems(); // Get selected items

    for (QListWidgetItem *item : selected) {
        selectedItems.push_back(item->text().toStdString());
    }
    return selectedItems;
}
