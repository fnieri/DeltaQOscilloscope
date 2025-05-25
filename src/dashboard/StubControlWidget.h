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
    // Erlang controls
    QPushButton *startErlangButton;
    QPushButton *stopErlangButton;

    // Server controls
    QPushButton *startServerButton;
    QPushButton *stopServerButton;
    QLineEdit *serverIpEdit;
    QLineEdit *serverPortEdit;

    // Erlang receiver settings
    QLineEdit *erlangReceiverIpEdit;
    QLineEdit *erlangReceiverPortEdit;
    QPushButton *setErlangEndpointButton;

    QVBoxLayout *mainLayout;

private Q_SLOTS:
    // Erlang slots
    void onStartErlangClicked();
    void onStopErlangClicked();

    // Server slots
    void onStartServerClicked();
    void onStopServerClicked();

    // Erlang endpoint slot
    void onSetErlangEndpointClicked();
};
