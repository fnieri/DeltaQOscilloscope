
#include "Application.h"
#include "dashboard/MainWindow.h"
#include "diagram/System.h"
#include "server/ErlangSender.h"
#include "server/Server.h"
#include <QApplication>
#include <cstring>

#include <QApplication>
#include <QPalette>
#include <QStyleFactory>

void setLightMode(QApplication &app)
{
    // Use the Fusion style (consistent across platforms)
    app.setStyle(QStyleFactory::create("Fusion"));

    QPalette lightPalette;

    lightPalette.setColor(QPalette::Window, QColor(255, 255, 255));
    lightPalette.setColor(QPalette::WindowText, Qt::black);
    lightPalette.setColor(QPalette::Base, QColor(245, 245, 245));
    lightPalette.setColor(QPalette::AlternateBase, QColor(240, 240, 240));
    lightPalette.setColor(QPalette::ToolTipBase, Qt::white);
    lightPalette.setColor(QPalette::ToolTipText, Qt::black);
    lightPalette.setColor(QPalette::Text, Qt::black);
    lightPalette.setColor(QPalette::Button, QColor(230, 230, 230));
    lightPalette.setColor(QPalette::ButtonText, Qt::black);
    lightPalette.setColor(QPalette::BrightText, Qt::red);
    lightPalette.setColor(QPalette::Link, QColor(0, 122, 204));

    lightPalette.setColor(QPalette::Highlight, QColor(0, 122, 204));
    lightPalette.setColor(QPalette::HighlightedText, Qt::white);

    app.setPalette(lightPalette);
}

int main(int argc, char *argv[])
{
    Application &application = Application::getInstance();

    System system = System();

    Server server(8080);
    server.start();

    application.setSystem(system);
    QApplication app(argc, argv);
    setLightMode(app);
    MainWindow window;
    window.show();

    int result = app.exec();

    return result;
}
