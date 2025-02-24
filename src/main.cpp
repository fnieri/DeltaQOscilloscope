
#include "dashboard/MainWindow.h"
#include "diagram/Outcome.h"
#include "diagram/System.h"
#include "server/Server.h"
#include <QApplication>
#include <memory>
#include <thread>

int main(int argc, char *argv[])
{

    // Create a shared System instance
    std::shared_ptr<System> system = std::make_shared<System>();
    std::unordered_map<std::string, std::shared_ptr<Outcome>> outcomes;
    auto outcome1 = std::make_shared<Outcome>("worker_1");
    auto outcome2 = std::make_shared<Outcome>("worker_2");
    outcomes["worker_1"] = outcome1;
    outcomes["worker_2"] = outcome2;

    system->setOutcomes(outcomes);

    std::unordered_map<std::string, std::shared_ptr<Probe>> probes;
    auto probe1 = std::make_shared<Probe>("full_loop");
    probes["full_loop"] = probe1;
    system->setProbes(probes);
    // Start the server in a separate thread
    Server server(8080, system);
    server.start();

    QApplication app(argc, argv);

    // Create the Dashboard widget
    // Dashboard dashboard;
    // dashboard.setWindowTitle("System Editor");
    // dashboard.resize(400, 300); // Set an appropriate size
    // dashboard.show();
    MainWindow window(system);
    window.show();

    return app.exec(); // Start Qt event loop
}
