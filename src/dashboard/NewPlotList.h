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
    explicit NewPlotList(QWidget *parent = nullptr);

    std::vector<std::string> getSelectedItems();

    void deselectAll();
    void reset();

private:
    void addCategory(const QString &category);

    void addItems();
    void addProbes(std::unordered_map<std::string, std::shared_ptr<Probe>> probes);
    void addOutcomes(std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomes);
    void addOperators(std::unordered_map<std::string, std::shared_ptr<Operator>> operators);
};

#endif // NEW_PLOT_LIST_H
