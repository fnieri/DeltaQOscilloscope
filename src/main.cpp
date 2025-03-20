
#include "dashboard/Application.h"
#include "dashboard/MainWindow.h"
#include "diagram/Outcome.h"
#include "diagram/System.h"
#include "server/Server.h"
#include <QApplication>
#include <memory>
#include <thread>
int main(int argc, char *argv[])
{
    Application &application = Application::getInstance();
    // Create a shared System instance
    System system = System();
    std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomes;
    auto outcome1 = std::make_shared<Outcome>("worker_1");
    auto outcome2 = std::make_shared<Outcome>("worker_2");
    outcomes["worker_1"] = outcome1;
    outcomes["worker_2"] = outcome2;
    outcome1->setNext("full_loop", outcome2);
    system.setOutcomes(outcomes);

    std::unordered_map<std::string, std::shared_ptr<Probe>> probes;
    auto probe1 = std::make_shared<Probe>("full_loop");
    probes["full_loop"] = probe1;
    probe1->setFirstComponent(outcome1);
    system.setProbes(probes);
    application.setSystem(system);
    // Start the server in a separate thread
    Server server(8080, application.getSystem());
    server.start();

    QApplication app(argc, argv);

    // Create the Dashboard widget
    // Dashboard dashboard;
    // dashboard.setWindowTitle("System Editor");
    // dashboard.resize(400, 300); // Set an appropriate size
    // dashboard.show();
    MainWindow window;
    window.show();

    return app.exec(); // Start Qt event loop
}
