#include "StubControlWidget.h"

StubControlWidget::StubControlWidget(QWidget *parent)
    : QWidget(parent)
{
    mainLayout = new QVBoxLayout(this);

    // Erlang Control Group
    QGroupBox *erlangGroup = new QGroupBox("Erlang Wrapper Control", this);
    QVBoxLayout *erlangMainLayout = new QVBoxLayout(erlangGroup);

    // First row: IP and Port
    QHBoxLayout *erlangIpPortLayout = new QHBoxLayout();
    erlangIpPortLayout->addWidget(new QLabel("IP:"));
    erlangReceiverIpEdit = new QLineEdit("127.0.0.1", this);
    erlangIpPortLayout->addWidget(erlangReceiverIpEdit);
    erlangIpPortLayout->addWidget(new QLabel("Port:"));
    erlangReceiverPortEdit = new QLineEdit("8081", this);
    erlangIpPortLayout->addWidget(erlangReceiverPortEdit);
    erlangMainLayout->addLayout(erlangIpPortLayout);

    // Second row: Set Endpoint Button
    setErlangEndpointButton = new QPushButton("Set Adapter Endpoint", this);
    erlangMainLayout->addWidget(setErlangEndpointButton, 0, Qt::AlignLeft);

    // Third row: Start/Stop Wrapper
    QHBoxLayout *erlangButtonsLayout = new QHBoxLayout();
    stopErlangButton = new QPushButton("Stop Adapter", this);
    startErlangButton = new QPushButton("Start Adapter", this);
    erlangButtonsLayout->addWidget(stopErlangButton);
    erlangButtonsLayout->addWidget(startErlangButton);
    erlangMainLayout->addLayout(erlangButtonsLayout);

    // Server Control Group
    QGroupBox *serverGroup = new QGroupBox("C++ Server Control", this);
    QVBoxLayout *serverMainLayout = new QVBoxLayout(serverGroup);

    // First row: IP and Port
    QHBoxLayout *serverIpPortLayout = new QHBoxLayout();
    serverIpPortLayout->addWidget(new QLabel("IP:"));
    serverIpEdit = new QLineEdit("0.0.0.0", this);
    serverIpPortLayout->addWidget(serverIpEdit);
    serverIpPortLayout->addWidget(new QLabel("Port:"));
    serverPortEdit = new QLineEdit("8080", this);
    serverIpPortLayout->addWidget(serverPortEdit);
    serverMainLayout->addLayout(serverIpPortLayout);

    // Second row: Start/Stop Server
    QHBoxLayout *serverButtonsLayout = new QHBoxLayout();
    startServerButton = new QPushButton("Start Oscilloscope Server", this);
    stopServerButton = new QPushButton("Stop Oscilloscope Server", this);
    serverButtonsLayout->addWidget(startServerButton);
    serverButtonsLayout->addWidget(stopServerButton);
    serverMainLayout->addLayout(serverButtonsLayout);

    // Add to main layout
    mainLayout->addWidget(erlangGroup);
    mainLayout->addWidget(serverGroup);
    mainLayout->addStretch();
    setLayout(mainLayout);
    setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Maximum);

    // Connect signals
    connect(startErlangButton, &QPushButton::clicked, this, &StubControlWidget::onStartErlangClicked);
    connect(stopErlangButton, &QPushButton::clicked, this, &StubControlWidget::onStopErlangClicked);
    connect(startServerButton, &QPushButton::clicked, this, &StubControlWidget::onStartServerClicked);
    connect(stopServerButton, &QPushButton::clicked, this, &StubControlWidget::onStopServerClicked);
    connect(setErlangEndpointButton, &QPushButton::clicked, this, &StubControlWidget::onSetErlangEndpointClicked);
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
    QString ip = serverIpEdit->text();
    int port = serverPortEdit->text().toInt();

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

void StubControlWidget::onSetErlangEndpointClicked()
{
    QString ip = erlangReceiverIpEdit->text();
    int port = erlangReceiverPortEdit->text().toInt();

    Application::getInstance().setErlangEndpoint(ip.toStdString(), port);
}
