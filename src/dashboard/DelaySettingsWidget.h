
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

private:
    QComboBox *observableComboBox;
    QSlider *delaySlider;
    QSpinBox *binSpinBox;
    QLabel *maxDelayLabel;
    QPushButton *saveDelayButton;
};
