#pragma once

#include <QCheckBox>
#include <QComboBox>
#include <QFormLayout>
#include <QListWidget>
#include <QMap>
#include <QSpinBox>
#include <QVBoxLayout>
#include <QWidget>

#include "../Application.h"
#include "src/maths/TriggerManager.h"

class TriggersTab : public QWidget
{
    Q_OBJECT

public:
    explicit TriggersTab(QWidget *parent = nullptr);
    ~TriggersTab();
    void addTriggeredMessage(const QString &msg); // To display in list
private Q_SLOTS:
    void onObservableChanged(const QString &name);
    void onTriggerChanged();
    void onTriggeredItemClicked(QListWidgetItem *item);

private:
    void captureSnapshots(std::uint64_t, const std::string &);

    void populateObservables();
    void updateCheckboxStates();

    QVBoxLayout *mainLayout;
    QFormLayout *formLayout;
    QComboBox *observableComboBox;

    QWidget *sampleLimitWidget;
    QHBoxLayout *sampleLimitLayout;
    QCheckBox *sampleLimitCheckBox;
    QSpinBox *sampleLimitSpinBox;

    QCheckBox *qtaBoundsCheckBox;
    QCheckBox *failureRateCheckBox;

    QListWidget *triggeredList;

    std::shared_ptr<Observable> getCurrentObservable();

    static constexpr int sampleLimitThreshold = 500;
    static constexpr double failureRateThreshold = 0.95;
};
