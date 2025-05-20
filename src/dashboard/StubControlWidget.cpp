#include "StubControlWidget.h"

StubControlWidget::StubControlWidget(QWidget *parent)
    : QWidget(parent)
{
    startButton = new QPushButton("Start Stub", this);
    stopButton = new QPushButton("Stop Stub", this);

    layout = new QHBoxLayout(this);
    layout->addWidget(startButton);
    layout->addWidget(stopButton);

    setLayout(layout);

    connect(startButton, &QPushButton::clicked, this, &StubControlWidget::onStartClicked);
    connect(stopButton, &QPushButton::clicked, this, &StubControlWidget::onStopClicked);
}
