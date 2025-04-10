
#pragma once

#ifndef DQ_PLOT_LIST_H
#define DQ_PLOT_LIST_H

#include "DQPlotController.h"
#include <QCheckBox>
#include <QListWidget>
#include <QPushButton>
#include <QVBoxLayout>
#include <QWidget>
class DQPlotController;
class DQPlotList : public QWidget
{
    Q_OBJECT
public:
    explicit DQPlotList(DQPlotController *controller, QWidget *parent = nullptr);

    void reset();

private slots:
    void onConfirmSelection();
    void onRemoveSelection();

private:
    void addCategory(const QString &category);
    void addItemToList(const std::string &name, bool isSelected, const QString &category);
    void updateLists();

    DQPlotController *controller;

    QListWidget *selectedList;
    QListWidget *availableList;

    QPushButton *addButton;
    QPushButton *removeButton;
};

#endif // DQ_PLOT_LIST_H
