
#include "Application.h"
#include "dashboard/MainWindow.h"
#include "diagram/System.h"
#include "server/ErlangSender.h"
#include "server/Server.h"
#include <QApplication>
#include <cstring>
int main(int argc, char *argv[])
{
    Application &application = Application::getInstance();

    System system = System();

    Server server(8080);
    server.start();
    application.setSystem(system);
    QApplication app(argc, argv);
    MainWindow window;
    window.show();

    int result = app.exec();

    return result;
}
