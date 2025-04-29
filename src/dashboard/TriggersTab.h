#pragma once

#include <QWidget>
#include <QComboBox>
#include <QCheckBox>
#include <QFormLayout>
#include <QVBoxLayout>
#include <QListWidget>
#include <QMap>

#include "../Application.h"
#include "src/maths/TriggerManager.h"

class TriggersTab : public QWidget {
    Q_OBJECT

public:
    explicit TriggersTab(QWidget *parent = nullptr);
    ~TriggersTab();
    void addTriggeredMessage(const QString& msg); // To display in list

private Q_SLOTS:
    void onObservableChanged(const QString& name);
    void onTriggerChanged();
    void onTriggeredItemClicked(QListWidgetItem* item);

private:
    void populateObservables();
    void updateCheckboxStates();

    QVBoxLayout *mainLayout;
    QFormLayout *formLayout;
    QComboBox* observableComboBox;
    QCheckBox* sampleLimitCheckBox;
    QCheckBox* qtaBoundsCheckBox;
    QCheckBox* failureRateCheckBox;

    QListWidget* triggeredList;

    TriggerManager triggerManagerForCurrentObservable();

    static constexpr int sampleLimitThreshold = 500;
    static constexpr double failureRateThreshold = 0.95;
};
