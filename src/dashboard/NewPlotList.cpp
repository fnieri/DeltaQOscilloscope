#include "NewPlotList.h"
#include "../Application.h"
#include <iostream>
#include <qlistwidget.h>

using Outcomes = std::unordered_map<std::string, std::shared_ptr<Outcome>>;
using Operators = std::unordered_map<std::string, std::shared_ptr<Operator>>;
using Probes = std::unordered_map<std::string, std::shared_ptr<Probe>>;

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

void NewPlotList::addCategory(const QString &category)
{
    QListWidgetItem *categoryItem = new QListWidgetItem(category, this);
    categoryItem->setFlags(Qt::NoItemFlags);
    categoryItem->setFont(QFont("Arial", 10, QFont::Bold)); // Bold label
}

void NewPlotList::addItems()
{
    auto system = Application::getInstance().getSystem();
    Probes probes = system->getProbes();
    Outcomes outcomes = system->getOutcomes();
    addProbes(probes);
    addOutcomes(outcomes);
}

void NewPlotList::addProbes(Probes probes)
{
    QString category = "Probes:";
    addCategory(category);

    for (const auto &probe : probes) {
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(probe.first), this);
        item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
    }
}

void NewPlotList::addOutcomes(Outcomes outcomes)
{
    QString category = "Outcomes:";
    addCategory(category);

    for (const auto &outcome : outcomes) {
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(outcome.first), this);
        item->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
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
