#pragma once
#include "src/Application.h"
#include <QApplication>
#include <QGridLayout>
#include <QGroupBox>
#include <QHBoxLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QVBoxLayout>
#include <QWidget>

class StubControlWidget : public QWidget
{
    Q_OBJECT
public:
    StubControlWidget(QWidget *parent = nullptr);

private:
    QPushButton *startErlangButton;
    QPushButton *stopErlangButton;

    QPushButton *startServerButton;
    QPushButton *stopServerButton;
    QLineEdit   *serverIpEdit;
    QLineEdit   *serverPortEdit;

    QVBoxLayout *mainLayout;

private Q_SLOTS:
    void onStartErlangClicked();
    void onStopErlangClicked();
    void onStartServerClicked();
    void onStopServerClicked();
};
