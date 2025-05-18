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
    void addItems();
};

#endif // NEW_PLOT_LIST_H
