#include "DQPlotController.h"
#include "../maths/DeltaQOperations.h"
#include "AddPlotDialog.h"

DQPlotController::DQPlotController(std::shared_ptr<System> system, DeltaQPlot *plot, SelectionResult selection)
    : system {system}
    , plot {plot}
{

    if (selection.selectedOperation == "Convolution") {
        operation = &convolveN;
    } else if (selection.selectedOperation == "All-to-Finish") {
        operation = &allToFinish;
    } else if (selection.selectedOperation == "First-to-Finish") {
        operation = &firstToFinish;
    }
    // Add outcomes
    for (const QString &item : selection.selectedOutcomes) {
        addComponent(item.toStdString(), false);
    }

    // Add probes
    for (const QString &item : selection.selectedProbes) {
        addComponent(item.toStdString(), true);
    }
}

bool DQPlotController::containsComponent(std::string name)
{
    return ((outcomes.find(name) != outcomes.end()) || (probes.find(name) != probes.end()));
}

void DQPlotController::editPlot(SelectionResult selection)
{

    QStringList existingItems;
    for (const auto &name : getComponents()) {
        existingItems.append(QString::fromStdString(name));
    }

    if (selection.selectedOperation == "Convolution") {
        operation = &convolveN;
    } else if (selection.selectedOperation == "All-to-Finish") {
        operation = &allToFinish;
    } else if (selection.selectedOperation == "First-to-Finish") {
        operation = &firstToFinish;
    }

    // Remove unselected items
    for (const QString &name : existingItems) {
        if (!selection.selectedOutcomes.contains(name) && !selection.selectedProbes.contains(name)) {
            removeComponent(name.toStdString());
        }
    }

    // Add new selections
    for (const QString &name : selection.selectedOutcomes) {
        std::string stdName = name.toStdString();
        if (!containsComponent(stdName)) {
            addComponent(stdName, false);
        }
    }
    for (const QString &name : selection.selectedProbes) {
        std::string stdName = name.toStdString();
        if (!containsComponent(stdName)) {
            addComponent(stdName, true);
        }
    }
}

void DQPlotController::addComponent(std::string name, bool isProbe)
{
    auto series = new QLineSeries();

    if (!isProbe) {
        outcomes[name] = {series, (system->getOutcome(name))};
    } else {
        probes[name] = {series, system->getProbe(name)};
    }

    plot->addSeries(series, name);
}

std::vector<std::string> DQPlotController::getComponents()
{
    std::vector<std::string> components;
    components.reserve(probes.size() + outcomes.size());

    for (auto kv : probes) {
        components.push_back(kv.first);
    }
    for (auto kv : outcomes) {
        components.push_back(kv.first);
    }

    return components;
}

void DQPlotController::removeComponent(std::string &&name)
{
    // Try to find the element in outcomes
    auto outcomePair = outcomes.find(name);
    if (outcomePair != outcomes.end()) {
        plot->removeSeries(outcomePair->second.first); // Use iterator to access value
        outcomes.erase(outcomePair); // Use iterator to erase
        return;
    }

    // Try to find the element in probes
    auto probesPair = probes.find(name);
    if (probesPair != probes.end()) {
        plot->removeSeries(probesPair->second.first);
        probes.erase(probesPair);
    }
}

void DQPlotController::update()
{

    double binWidth = system->getBinWidth();
    if (operation) {
        std::vector<DeltaQ> deltaQs;
        deltaQs.reserve(outcomes.size());
        for (auto &[name, seriesOutcome] : outcomes) {
            auto outcome = seriesOutcome.second;
            DeltaQ deltaQ = outcome->getDeltaQ(binWidth);
            deltaQs.push_back(deltaQ);
        }
        DeltaQ result = operation(deltaQs);

        std::vector<std::pair<double, double>> data;
        int size = result.getSize();

        data.reserve(size + 1);

        data.push_back({0, 0});
        for (int i = 0; i < size; i++) {
            data.push_back({binWidth * (i + 1), result.cdfAt(i)});
        }
        plot->updateOperationSeries(data, binWidth * size);

    } else {
        for (auto &[name, seriesOutcome] : outcomes) {
            updateComponent(seriesOutcome.first, seriesOutcome.second, binWidth);
        }
    }

    for (auto &[name, seriesProbe] : probes) {
        updateComponent(seriesProbe.first, seriesProbe.second, binWidth);
    }
}

void DQPlotController::updateComponent(QLineSeries *series, std::shared_ptr<Probe> component, double binWidth)
{
    std::vector<std::pair<double, double>> data;

    DeltaQ deltaQ = component->getDeltaQ(binWidth);
    int size = deltaQ.getSize();
    data.reserve(size + 1);

    data.push_back({0, 0});
    for (int i = 0; i < size; i++) {
        data.push_back({binWidth * (i + 1), deltaQ.cdfAt(i)});
    }

    plot->updateSeries(series, data, binWidth * size);
}
