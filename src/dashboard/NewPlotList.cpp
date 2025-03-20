#include "NewPlotList.h"
#include "Application.h"
#include <iostream>
#include <qlistwidget.h>

using Outcomes = std::unordered_map<std::string, std::shared_ptr<Outcome>>;
using Operators = std::unordered_map<std::string, std::shared_ptr<Operator>>;
using Probes = std::unordered_map<std::string, std::shared_ptr<Probe>>;

NewPlotList::NewPlotList(QWidget *parent)
    : QListWidget(parent)
{
    setSelectionMode(QAbstractItemView::MultiSelection);
    create();
    Application::getInstance().addObserver([this]() { this->reset(); });
}

void NewPlotList::create()
{
    addItems();
}

void NewPlotList::reset()
{

    // This right here is a hack, it sucks, but it works
    this->blockSignals(true);
    while (count() != 0)
        delete takeItem(0);
    this->blockSignals(false);

    create();
}

void NewPlotList::addCategory(const QString &category)
{
    QListWidgetItem *categoryItem = new QListWidgetItem(category, this);
    categoryItem->setFlags(Qt::NoItemFlags); // Non-selectable label
    categoryItem->setFont(QFont("Arial", 10, QFont::Bold)); // Bold label
}

void NewPlotList::addItems()
{
    auto system = Application::getInstance().getSystem();
    Probes probes = system->getProbes();
    Outcomes outcomes = system->getOutcomes();
    Operators operators = system->getOperators();
    addProbes(probes);
    addOutcomes(outcomes);
    addOperators(operators);
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

void NewPlotList::addOperators(Operators operators)
{
    QString category = "Operators:";
    addCategory(category);

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
