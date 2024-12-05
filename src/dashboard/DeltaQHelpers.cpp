//
// Created by francy on 05/12/24.
//
#include "DeltaQHelpers.h"

QLineSeries *toQSeries(const DeltaQ &deltaQ)
{
    auto *series = new QLineSeries();

    const double binWidth = deltaQ.getBinWidth();

    const std::vector<double>& cdfValues = deltaQ.getCdfValues();

    double current = 0;
    for (const double cdfValue : cdfValues) {
        series->append(current, cdfValue);
        current += binWidth;
    }
    return series;
}
