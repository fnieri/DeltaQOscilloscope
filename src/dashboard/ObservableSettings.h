#ifndef OBS_SETTINGS_H
#define OBS_SETTINGS_H

#include <qlabel.h>
#pragma once
#include <QWidget>

#include "DelaySettingsWidget.h"
#include "QTAInputWidget.h"

class ObservableSettings : public QWidget
{
    Q_OBJECT

    QVBoxLayout *layout;

    QLabel *delayLabel;
    DelaySettingsWidget *delaySettingsWidget;

    QLabel *qtaLabel;
    QTAInputWidget *qtaInputWidget;

public:
    explicit ObservableSettings(QWidget *parent = nullptr);
};

#endif
