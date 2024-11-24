#include "DeltaQ.h"
#include <iostream>
#include <vector>
#include <memory>

/**
 * Perform discrete convolution between two DeltaQs
 */
DeltaQ convolve(const DeltaQ& lhs, const DeltaQ& rhs) {
    // Determine the "higher" and "lower" DeltaQ objects based on their maximum values
    const DeltaQ& highestDeltaQ = (lhs > rhs) ? lhs : rhs;
    const DeltaQ& otherDeltaQ = (lhs > rhs) ? rhs : lhs;

    double binWidth = lhs.getBinWidth();

    // Calculate the total steps for the resulting PDF
    double totalSteps = (highestDeltaQ.getSize() * 2) - (highestDeltaQ.getSize() - otherDeltaQ.getSize());

    std::vector<double> convolutedPdf;
    convolutedPdf.reserve(totalSteps);

    // Perform convolution
    for (size_t i = 0; i < totalSteps; ++i) {
        double result = 0.0;

        // Sum the products of PDF values at offset bins
        for (size_t j = 0; j <= i; ++j) {
            result += highestDeltaQ.pdfAt(j) * otherDeltaQ.pdfAt(i - j);
        }

        // Normalize by bin width
        convolutedPdf.push_back(result * binWidth);
    }

    return DeltaQ(binWidth, convolutedPdf);
}
