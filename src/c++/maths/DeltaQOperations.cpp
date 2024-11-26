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
    double totalSteps = (highestDeltaQ.getSize() * 2) - (highestDeltaQ.getSize() - otherDeltaQ.getSize()) - 1;

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

    return DeltaQ(binWidth, convolutedPdf, true);
}

/**
 * Assume two independent outcomes with the same start event
 * All-to-finish outcome occurs when both end events occur
 * All-to-finish is defined as ΔQ_{LTF(A,B)} ΔQ_A * ΔQ_B
*/
DeltaQ allToFinish(const std::vector<DeltaQ>& deltaQs) {
    DeltaQ result = deltaQs[0];
    for (size_t i = 1; i < deltaQs.size(); ++i) {
        result = result * deltaQs[i];
    }
    return result;
}

/**
 * Assume two independent outcomes with the same start event
 * First-to-finish outcome occurs when at least one end event occurs
 * We compute the probability that there are zero end events
 * First-to-finish is defined as    
 * ΔQ_{FTF(A,B)} = ΔQ_A + ΔQ_B – ΔQ_A * ΔQ_B
*/
DeltaQ firstToFinish(const std::vector<DeltaQ>& deltaQs) {
    std::vector<double> resultingCdf;

    double sumAtI;
    double productAtI;
    double resultAtI;

    int binWidth = deltaQs[0].getBinWidth();
    int largestDeltaQSize = chooseLongestDeltaQSize(deltaQs);


    for (size_t i = 0; i < largestDeltaQSize; i++) {
        sumAtI = 0;
        productAtI = 0;
        for (DeltaQ deltaQ: deltaQs) {
            double cdfAtI = deltaQ.cdfAt(i);
            sumAtI += cdfAtI;
            productAtI *= cdfAtI;
        }
        resultAtI = sumAtI - productAtI;
        resultingCdf.push_back(resultAtI);
    }
    return DeltaQ(binWidth, resultingCdf, false);
}

/**
 *  Assume there are two possible outcomes OA and OB and
 * exactly one outcome is chosen during each occurrence of a start event
 * O_A occurs with probability p/(p+q)
 * O_B occurs with probability q/(p+q)
 * Therefore:
 * ΔQ_{PC(A,B)} = p/(p+q) ΔQ_A + q/(p+q) ΔQ_B
*/
DeltaQ probabilisticChoice(const std::vector<double>& probabilities, const std::vector<DeltaQ>& deltaQs) {
    std::vector<double> resultingCdf;

    double sumAtI;
    int binWidth = deltaQs[0].getBinWidth();
    
    int largestDeltaQSize = chooseLongestDeltaQSize(deltaQs);
    int noOfDeltaQs = deltaQs.size();
    std::vector<DeltaQ> scaledDeltaQs;

    for (size_t i = 0; i < noOfDeltaQs; i++) {
        scaledDeltaQs.push_back(deltaQs[i] * probabilities[i]); // Use operator* to scale
    }

    DeltaQ result = scaledDeltaQs[0];
    for (size_t i = 1; i < noOfDeltaQs; ++i) {
        result = result + deltaQs[i];
    }
}

/**
 * Choose the highest size from a list of DeltaQs
*/
int chooseLongestDeltaQSize(const std::vector<DeltaQ>& deltaQs) {
    int highestSize = 0;
    for (DeltaQ deltaQ: deltaQs) {
        if (deltaQ.getSize() > highestSize) {
            highestSize = deltaQ.getSize();
        }
    }
    return highestSize;
}