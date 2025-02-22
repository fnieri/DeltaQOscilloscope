#include "DeltaQOperations.h"
#include "DeltaQ.h"

#include <iostream>
#include <vector>

DeltaQ convolve(const DeltaQ &lhs, const DeltaQ &rhs)
{
    // If one of the two DeltaQs is empty, return the other DeltaQ
    if (lhs == DeltaQ()) {
        return rhs;
    }
    if (rhs == DeltaQ()) {
        return lhs;
    }

    // Determine the "higher" and "lower" DeltaQ objects based on their maximum values
    const DeltaQ &highestDeltaQ = (lhs > rhs) ? lhs : rhs;
    const DeltaQ &otherDeltaQ = (lhs > rhs) ? rhs : lhs;

    double binWidth = lhs.getBinWidth();
    const int totalSteps = (highestDeltaQ.getSize() * 2) - (highestDeltaQ.getSize() - otherDeltaQ.getSize()) - 1;

    std::vector<long double> convolutedPdf;
    convolutedPdf.reserve(totalSteps);
    // Perform convolution
    for (size_t i = 0; i < totalSteps; ++i) {
        double result = 0.0;

        // Sum the products of PDF values at offset bins
        for (size_t j = 0; j <= i; ++j) {
            result += highestDeltaQ.pdfAt(j) * otherDeltaQ.pdfAt(i - j);
        }

        // Normalize by bin width
        convolutedPdf.push_back(result);
    }
    return {binWidth, convolutedPdf, true};
}

DeltaQ convolveN(const std::vector<DeltaQ> &deltaQs)
{
    if (deltaQs.empty())
        return DeltaQ();

    DeltaQ result = deltaQs[0];

    for (size_t i = 1; i < deltaQs.size(); ++i) {
        result = convolve(result, deltaQs[i]);
    }
    return result;
}

DeltaQ allToFinish(const std::vector<DeltaQ> &deltaQs)
{
    DeltaQ result = deltaQs[0];
    for (size_t i = 1; i < deltaQs.size(); ++i) {
        result = result * deltaQs[i];
    }
    return result;
}

DeltaQ firstToFinish(const std::vector<DeltaQ> &deltaQs)
{
    std::vector<long double> resultingCdf;

    const double binWidth = deltaQs[0].getBinWidth();
    const int largestDeltaQSize = chooseLongestDeltaQSize(deltaQs);

    for (size_t i = 0; i < largestDeltaQSize; i++) {
        double sumAtI = 0;
        double productAtI = 1;
        for (const DeltaQ &deltaQ : deltaQs) {
            const double cdfAtI = deltaQ.cdfAt(i);
            sumAtI += cdfAtI;
            productAtI *= cdfAtI;
        }
        double resultAtI = sumAtI - productAtI;
        resultingCdf.push_back(resultAtI);
    }
    return {binWidth, resultingCdf, false};
}

DeltaQ probabilisticChoice(const std::vector<double> &probabilities, const std::vector<DeltaQ> &deltaQs)
{
    std::vector<long double> resultingCdf;

    double binWidth = deltaQs[0].getBinWidth();

    const int noOfDeltaQs = deltaQs.size();
    std::vector<DeltaQ> scaledDeltaQs;

    for (size_t i = 0; i < noOfDeltaQs; i++) {
        scaledDeltaQs.push_back(deltaQs[i] * probabilities[i]); // Use operator* to scale
    }

    DeltaQ result = scaledDeltaQs[0];
    for (size_t i = 1; i < noOfDeltaQs; ++i) {
        result = result + deltaQs[i];
    }
    return {binWidth, resultingCdf, true};
}

int chooseLongestDeltaQSize(const std::vector<DeltaQ> &deltaQs)
{
    int highestSize = 0;
    for (const DeltaQ &deltaQ : deltaQs) {
        if (deltaQ.getSize() > highestSize) {
            highestSize = deltaQ.getSize();
        }
    }
    return highestSize;
}
