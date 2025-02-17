
#include "DeltaQPlot.h"
#include <qlineseries.h>

DeltaQPlot::DeltaQPlot(std::shared_ptr<System> system, QWidget *parent, DeltaQ (*operation)(const std::vector<DeltaQ> &))
    : QChartView(parent)
    , chart(new QChart())
    , system {system}
    , operation {operation}
{
    chart->legend()->setVisible(true);

    axisX = new QValueAxis();
    axisY = new QValueAxis();
    axisY->setRange(0, 1.0);
    chart->addAxis(axisX, Qt::AlignBottom);
    chart->addAxis(axisY, Qt::AlignLeft);
    setChart(chart);
    operationSeries = new QLineSeries();
    chart->addSeries(operationSeries);
    operationSeries->attachAxis(axisX);
    operationSeries->attachAxis(axisY);
}

std::vector<std::string> DeltaQPlot::getComponents()
{
    std::vector<std::string> keys;
    keys.reserve(probes.size() + outcomes.size());

    for (auto kv : probes) {
        keys.push_back(kv.first);
    }
    for (auto kv : outcomes) {
        keys.push_back(kv.first);
    }

    return keys;
}

bool DeltaQPlot::containsComponent(std::string name)
{
    return ((outcomes.find(name) != outcomes.end()) || (probes.find(name) != probes.end()));
}

void DeltaQPlot::addComponent(std::string name, bool isProbe)
{
    auto series = new QLineSeries();

    if (!isProbe) {
        outcomes[name] = {series, (system->getOutcome(name))};
    } else {
        probes[name] = {series, system->getProbe(name)};
    }
    chart->addSeries(series);
    series->setName(QString::fromStdString(name));

    series->attachAxis(axisX);
    series->attachAxis(axisY);
}

void DeltaQPlot::removeComponent(std::string name)
{
    auto outcomePair = outcomes.find(name);
    if (outcomePair != outcomes.end()) {
        chart->removeSeries(outcomes.at(name).first);
        outcomes.erase(name);
        return;
    }
    auto probesPair = probes.find(name);
    if (probesPair != probes.end()) {
        chart->removeSeries(probes.at(name).first);
        probes.erase(name);
    }
}

void DeltaQPlot::update()
{
    // Get system at update time, so all outcomes will share
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
        operationSeries->clear();
        operationSeries->append(0, 0);
        int size = result.getSize();
        for (int i = 0; i < size; i++) {
            operationSeries->append(binWidth * (i + 1), result.cdfAt(i));
        }
        axisX->setRange((0), binWidth * size);
    } else {
        for (auto &[name, seriesOutcome] : outcomes) {

            auto series = seriesOutcome.first;
            auto outcome = seriesOutcome.second;
            series->clear();

            DeltaQ deltaQ = outcome->getDeltaQ(binWidth);
            series->append(0, 0);

            int size = deltaQ.getSize();
            for (int i = 0; i < size; i++) {
                series->append(binWidth * (i + 1), deltaQ.cdfAt(i));
            }
            axisX->setRange(0, binWidth * size);
        }
    }

    for (auto &[name, seriesProbe] : probes) {
        auto series = seriesProbe.first;
        auto outcome = seriesProbe.second;
        series->clear();

        DeltaQ deltaQ = outcome->getDeltaQ(binWidth);
        series->append(0, 0);

        int size = deltaQ.getSize();
        for (int i = 0; i < size; i++) {
            series->append(binWidth * (i + 1), deltaQ.cdfAt(i));
        }
        axisX->setRange(0, binWidth * size);
    }
}

void DeltaQPlot::setOperation(DeltaQ (*newOperation)(const std::vector<DeltaQ> &))
{
    operation = newOperation;
    update(); // Refresh the plot
}
