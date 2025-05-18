#include "ObservableSettings.h"
#include <qlabel.h>
#include <qwidget.h>

ObservableSettings::ObservableSettings(QWidget *parent)
    : QWidget(parent)
{
    layout = new QVBoxLayout(this);
    layout->setAlignment(Qt::AlignTop);
    layout->setSpacing(10);
    layout->setContentsMargins(10, 10, 10, 10);

    qtaInputWidget = new QTAInputWidget(this);
    layout->addWidget(qtaInputWidget);

    delaySettingsWidget = new DelaySettingsWidget(this);
    layout->addWidget(delaySettingsWidget);
    connect(delaySettingsWidget, &DelaySettingsWidget::delayParametersChanged, qtaInputWidget, &QTAInputWidget::loadObservableSettings);
}
