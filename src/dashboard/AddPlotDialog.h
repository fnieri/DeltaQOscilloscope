
#ifndef ADDPLOTDIALOG_H
#define ADDPLOTDIALOG_H

#pragma once

#include "../diagram/System.h"
#include <QDialog>
#include <QListWidget>
#include <QPushButton>
#include <memory>
#include <qcontainerfwd.h>

struct SelectionResult {
    QStringList selectedOutcomes;
    QStringList selectedProbes;
    QString selectedOperation;
};

class AddPlotDialog : public QDialog
{
    Q_OBJECT

public:
    explicit AddPlotDialog(std::shared_ptr<System> system, const QStringList &preselectedItem, QWidget *parent = nullptr);
    QStringList getSelectedItems() const;
    SelectionResult getSelections() const;

private:
    QListWidget *listWidget;
    QListWidget *probesWidget;
    QListWidget *operationsWidget;
    QPushButton *okButton;
};

#endif // ADDPLOTDIALOG_H
