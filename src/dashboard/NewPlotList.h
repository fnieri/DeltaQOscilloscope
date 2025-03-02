#pragma once

#ifndef NEW_PLOT_LIST_H
#define NEW_PLOT_LIST_H

#include "../diagram/System.h"
#include <QCheckBox>
#include <QListWidget>
#include <qlistwidget.h>
#include <qwidget.h>
class NewPlotList : public QListWidget
{
    Q_OBJECT
public:
    explicit NewPlotList(std::shared_ptr<System> system, QWidget *parent = nullptr);

    std::vector<std::string> getSelectedItems();

    void deselectAll();

private:
    std::shared_ptr<System> system;

    void create();
    void addCategory(const QString &category);

    void addItems();
    void addProbes();
    void addOutcomes();
    void addOperators();

    QMap<QString, QListWidgetItem *> systemItems;
};

#endif // NEW_PLOT_LIST_H
