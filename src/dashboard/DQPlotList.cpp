
#include "DQPlotList.h"
#include "../Application.h"
#include <QLabel>
#include <QListWidgetItem>
#include <qlistwidget.h>
DQPlotList::DQPlotList(DQPlotController *controller, QWidget *parent)
    : QWidget(parent)
    , controller(controller)
{
    QVBoxLayout *layout = new QVBoxLayout(this);

    // Selected items list
    QLabel *selectedLabel = new QLabel("Selected Components:");
    selectedList = new QListWidget(this);
    layout->addWidget(selectedLabel);
    layout->addWidget(selectedList);
    selectedList->setSelectionMode(QAbstractItemView::MultiSelection);
    removeButton = new QPushButton("Remove selected components", this);
    layout->addWidget(removeButton);

    connect(removeButton, &QPushButton::clicked, this, &DQPlotList::onRemoveSelection);
    // Available items list
    QLabel *availableLabel = new QLabel("Available Components:");
    availableList = new QListWidget(this);
    layout->addWidget(availableLabel);
    layout->addWidget(availableList);
    availableList->setSelectionMode(QAbstractItemView::MultiSelection);
    // Confirm button
    addButton = new QPushButton("Add selected components", this);
    connect(addButton, &QPushButton::clicked, this, &DQPlotList::onConfirmSelection);
    layout->addWidget(addButton);

    updateLists();
}
bool DQPlotList::isEmptyAfterReset()
{
    return controller->isEmptyAfterReset();
}

void DQPlotList::addCategory(const QString &category)
{
    QListWidgetItem *categoryItem = new QListWidgetItem(category, availableList);
    categoryItem->setFlags(Qt::NoItemFlags); // Non-selectable
    categoryItem->setFont(QFont("Arial", 10, QFont::Bold));
}

void DQPlotList::updateLists()
{
    availableList->clear();
    selectedList->clear();

    auto plotComponents = controller->getComponents();
    auto system = Application::getInstance().getSystem();
    // Add components selected in a DeltaQPlot
    for (auto &component : plotComponents) {
        new QListWidgetItem(QString::fromStdString(component), selectedList);
    }

    // Select all components that are available to be chosen to add
    auto allComponents = system->getAllComponentsName();

    auto pred
        = [&plotComponents](const std::string &key) -> bool { return std::find(plotComponents.begin(), plotComponents.end(), key) != plotComponents.end(); };

    allComponents.erase(std::remove_if(allComponents.begin(), allComponents.end(), pred), allComponents.end());
    for (auto &component : allComponents) {
        new QListWidgetItem(QString::fromStdString(component), availableList);
    }
}

void DQPlotList::onConfirmSelection()
{
    QList<QListWidgetItem *> selected = availableList->selectedItems();
    auto system = Application::getInstance().getSystem();
    for (QListWidgetItem *item : selected) {
        controller->addComponent(item->text().toStdString(), system->hasProbe(item->text().toStdString()));
    }
    updateLists();
}

void DQPlotList::onRemoveSelection()
{
    QList<QListWidgetItem *> selected = selectedList->selectedItems();

    for (QListWidgetItem *item : selected) {
        controller->removeComponent(item->text().toStdString());
    }
    updateLists();
}
