#pragma once

#include <QVBoxLayout>
#include <QWidget>
#include <map>
#include <qscrollarea.h>
class CustomLegendPanel : public QWidget
{
    Q_OBJECT
public:
    CustomLegendPanel(QWidget *parent = nullptr);
    void addEntry(const QString &name, const QColor &color);
    void removeEntry(const QString &name);
    void clear();

private:
    std::map<QString, QWidget *> legendEntries;
    QVBoxLayout *mainLayout;
    QVBoxLayout *legendLayout;
    QScrollArea *scrollArea;
    QWidget *scrollContent;
};
