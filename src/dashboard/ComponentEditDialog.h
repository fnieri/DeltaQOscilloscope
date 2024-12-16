#pragma once

#include "JsonComponent.h"
#include <QComboBox>
#include <QDialog>
#include <QLineEdit>
#include <QPushButton>
#include <nlohmann/json.hpp>

class ComponentEditDialog : public QDialog
{
    Q_OBJECT

public:
    explicit ComponentEditDialog(QWidget *parent = nullptr);
    void setFields(const JsonComponent &component);
    JsonComponent getEditedComponent() const;

private:
    QLineEdit *nameLineEdit;
    QLineEdit *startEventLineEdit;
    QLineEdit *endEventLineEdit;
    QComboBox *operatorChoice;
    QPushButton *confirmButton;
    QPushButton *cancelButton;

private slots:
    void onConfirm();
};
