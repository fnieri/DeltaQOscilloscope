#include "StubControlWidget.h"

StubControlWidget::StubControlWidget(QWidget *parent)
    : QWidget(parent)
{
    mainLayout = new QVBoxLayout(this);

    // Erlang adapter control — sends start_stub / stop_stub on the active connection
    QGroupBox *erlangGroup = new QGroupBox("Erlang Wrapper Control", this);
    QHBoxLayout *erlangButtonsLayout = new QHBoxLayout(erlangGroup);
    startErlangButton = new QPushButton("Start Adapter", this);
    stopErlangButton  = new QPushButton("Stop Adapter",  this);
    erlangButtonsLayout->addWidget(startErlangButton);
    erlangButtonsLayout->addWidget(stopErlangButton);

    // C++ server control — Erlang connects inbound here
    QGroupBox *serverGroup = new QGroupBox("C++ Server Control", this);
    QVBoxLayout *serverMainLayout = new QVBoxLayout(serverGroup);

    QHBoxLayout *serverIpPortLayout = new QHBoxLayout();
    serverIpPortLayout->addWidget(new QLabel("IP:"));
    serverIpEdit = new QLineEdit("0.0.0.0", this);
    serverIpPortLayout->addWidget(serverIpEdit);
    serverIpPortLayout->addWidget(new QLabel("Port:"));
    serverPortEdit = new QLineEdit("8080", this);
    serverIpPortLayout->addWidget(serverPortEdit);
    serverMainLayout->addLayout(serverIpPortLayout);

    QHBoxLayout *serverButtonsLayout = new QHBoxLayout();
    startServerButton = new QPushButton("Start Oscilloscope Server", this);
    stopServerButton  = new QPushButton("Stop Oscilloscope Server",  this);
    serverButtonsLayout->addWidget(startServerButton);
    serverButtonsLayout->addWidget(stopServerButton);
    serverMainLayout->addLayout(serverButtonsLayout);

    mainLayout->addWidget(erlangGroup);
    mainLayout->addWidget(serverGroup);
    mainLayout->addStretch();
    setLayout(mainLayout);
    setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Maximum);

    connect(startErlangButton, &QPushButton::clicked, this, &StubControlWidget::onStartErlangClicked);
    connect(stopErlangButton,  &QPushButton::clicked, this, &StubControlWidget::onStopErlangClicked);
    connect(startServerButton, &QPushButton::clicked, this, &StubControlWidget::onStartServerClicked);
    connect(stopServerButton,  &QPushButton::clicked, this, &StubControlWidget::onStopServerClicked);
}

void StubControlWidget::onStartErlangClicked()
{
    Application::getInstance().setStubRunning(true);
}

void StubControlWidget::onStopErlangClicked()
{
    Application::getInstance().setStubRunning(false);
}

void StubControlWidget::onStartServerClicked()
{
    QString ip   = serverIpEdit->text();
    int     port = serverPortEdit->text().toInt();

    if (Application::getInstance().startCppServer(ip.toStdString(), port)) {
        startServerButton->setEnabled(false);
        stopServerButton->setEnabled(true);
    }
}

void StubControlWidget::onStopServerClicked()
{
    Application::getInstance().stopCppServer();
    startServerButton->setEnabled(true);
    stopServerButton->setEnabled(false);
}
