#pragma once

#include <QVBoxLayout>
#include <QWidget>
#include <map>
#include <qscrollarea.h>

/**
 * @class CustomLegendPanel
 * @brief A scrollable widget that displays a legend for a plot.
 */
class CustomLegendPanel : public QWidget
{
    Q_OBJECT

public:
    explicit CustomLegendPanel(QWidget *parent = nullptr);

    /**
     * @brief Adds a new entry to the legend.
     * @param name The display name for the entry.
     * @param color The color for the entry.
     */
    void addEntry(const QString &name, const QColor &color);

    /**
     * @brief Removes an entry from the legend by name.
     * @param name The name of the entry to remove.
     */
    void removeEntry(const QString &name);

    /**
     * @brief Clears all entries from the legend.
     */
    void clear();

private:
    std::map<QString, QWidget *> legendEntries;  ///< Map of legend entries by name
    QVBoxLayout *mainLayout;                    ///< Main layout of the panel
    QVBoxLayout *legendLayout;                  ///< Layout containing the legend entries
    QScrollArea *scrollArea;                    ///< Scroll area for the legend content
    QWidget *scrollContent;                     ///< Widget containing the scrollable content
};
