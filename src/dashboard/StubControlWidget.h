
#pragma once

#include "src/Application.h"
#include <QApplication>
#include <QHBoxLayout>
#include <QPushButton>
#include <QWidget>
#include <qpushbutton.h>

class StubControlWidget : public QWidget
{
    Q_OBJECT

public:
    StubControlWidget(QWidget *parent = nullptr);

    QPushButton *startButton;
    QPushButton *stopButton;
    QHBoxLayout *layout;

private Q_SLOTS:
    void onStartClicked()
    {
        Application::getInstance().setStubRunning(true);
    }

    void onStopClicked()
    {
        Application::getInstance().setStubRunning(false);
    }
};
