
#include "CustomLegendPanel.h"
#include "CustomLegendEntry.h"
CustomLegendPanel::CustomLegendPanel(QWidget *parent)
    : QWidget(parent)
{

    scrollArea = new QScrollArea(this);
    scrollArea->setWidgetResizable(true);
    scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    scrollArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

    scrollContent = new QWidget();
    scrollArea->setWidget(scrollContent);

    legendLayout = new QVBoxLayout(scrollContent); // Save this for addEntry()
    legendLayout->setAlignment(Qt::AlignTop);

    mainLayout = new QVBoxLayout(this);
    mainLayout->addWidget(scrollArea);
    setLayout(mainLayout);
}

void CustomLegendPanel::addEntry(const QString &name, const QColor &color)
{
    auto entry = new CustomLegendEntry(name, color, this);
    legendLayout->addWidget(entry);
    legendEntries[name] = entry;
}

void CustomLegendPanel::removeEntry(const QString &name)
{
    if (legendEntries.count(name)) {
        QWidget *entry = legendEntries[name];
        legendLayout->removeWidget(entry);
        entry->deleteLater();
        legendEntries.erase(name);
    }
}

void CustomLegendPanel::clear()
{
    for (auto it = legendEntries.begin(); it != legendEntries.end();) {
        QWidget *entry = it->second;
        legendLayout->removeWidget(entry);
        entry->deleteLater();
        it = legendEntries.erase(it);
    }
}
