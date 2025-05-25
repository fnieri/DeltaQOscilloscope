#include "CustomLegendPanel.h"
#include "CustomLegendEntry.h"

CustomLegendPanel::CustomLegendPanel(QWidget *parent)
    : QWidget(parent)
{
    // Initialize scroll area and configure its behavior
    scrollArea = new QScrollArea(this);
    scrollArea->setWidgetResizable(true);
    scrollArea->setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
    scrollArea->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

    // Create content widget for the scroll area
    scrollContent = new QWidget();
    scrollArea->setWidget(scrollContent);

    // Set up layout for legend entries (aligned to top)
    legendLayout = new QVBoxLayout(scrollContent);
    legendLayout->setAlignment(Qt::AlignTop);

    // Set up main layout and add scroll area
    mainLayout = new QVBoxLayout(this);
    mainLayout->addWidget(scrollArea);
    setLayout(mainLayout);
}

/**
 * @brief Adds a new entry to the legend panel.
 * @param name The name for the entry.
 * @param color The color for the entry.
 */
void CustomLegendPanel::addEntry(const QString &name, const QColor &color)
{
    auto entry = new CustomLegendEntry(name, color, this);
    legendLayout->addWidget(entry);
    legendEntries[name] = entry;
}

/**
 * @brief Removes a specific entry from the legend by name.
 * @param name The name of the entry to remove.
 */
void CustomLegendPanel::removeEntry(const QString &name)
{
    if (legendEntries.count(name)) {
        QWidget *entry = legendEntries[name];
        legendLayout->removeWidget(entry);
        entry->deleteLater();
        legendEntries.erase(name);
    }
}

/**
 * @brief Clears all entries from the legend panel.
 */
void CustomLegendPanel::clear()
{
    for (auto it = legendEntries.begin(); it != legendEntries.end();) {
        QWidget *entry = it->second;
        legendLayout->removeWidget(entry);
        entry->deleteLater();
        it = legendEntries.erase(it);
    }
}
