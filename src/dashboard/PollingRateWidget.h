#pragma once

#include <QLabel>
#include <QPushButton>
#include <QSlider>
#include <QVBoxLayout>
#include <QWidget>

class PollingRateWidget : public QWidget
{
    Q_OBJECT

public:
    explicit PollingRateWidget(QWidget *parent = nullptr);

private Q_SLOTS:
    void onSliderValueChanged(int value);
    void onSaveClicked();

Q_SIGNALS:
    void onPollingRateChanged(int milliseconds);

private:
    QSlider *slider;
    QLabel *valueLabel;
    QPushButton *saveButton;
    QVector<int> pollingRates;
};
