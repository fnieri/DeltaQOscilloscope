//
// Created by francy on 05/12/24.
//
#include "DeltaQHelpers.h"

QLineSeries *toQSeries(const DeltaQ &deltaQ)
{
    auto *series = new QLineSeries();

    const double binWidth = deltaQ.getBinWidth();

    const std::vector<long double> &cdfValues = deltaQ.getCdfValues();

    double current = 0;
    for (int i = 0; i < deltaQ.getSize(); i++) {
        series->append(deltaQ.getBinWidth() * i, deltaQ.cdfAt(i));
    }
    return series;
}
