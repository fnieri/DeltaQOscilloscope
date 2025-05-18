
#pragma once

#include <QComboBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSlider>
#include <QSpinBox>
#include <QVBoxLayout>
#include <QWidget>
#include <cmath>
#include <qboxlayout.h>
class DelaySettingsWidget : public QWidget
{
    Q_OBJECT

public:
    explicit DelaySettingsWidget(QWidget *parent = nullptr);

    void populateComboBox();

    double getMaxDelayMs() const;

Q_SIGNALS:
    void delayParametersChanged();
private Q_SLOTS:
    void updateMaxDelay();
    void onSaveDelayClicked();
    void loadObservableSettings();

private:
    QVBoxLayout *mainLayout;

    QLabel *settingsLabel;
    QLabel *maxDelayLabel;

    QHBoxLayout *settingsLayout;
    QComboBox *observableComboBox;
    QSlider *delaySlider;
    QSpinBox *binSpinBox;

    QPushButton *saveDelayButton;
};
