#include "DeltaQOperations.h"
#include "DeltaQ.h"
#include <fftw3.h>
#include <iostream>
#include <math.h>
#include <mutex>
#include <vector>
DeltaQ rebin(const DeltaQ &source, double targetBinWidth)
{
    double originalBinWidth = source.getBinWidth();

    if (std::abs(originalBinWidth - targetBinWidth) < 1e-9) {
        return source; // Already same bin width
    }

    if (targetBinWidth < originalBinWidth) {
        throw std::invalid_argument("Target bin width must be greater than or equal to original.");
    }

    int factor = static_cast<int>(std::round(targetBinWidth / originalBinWidth));
    const auto &originalPdf = source.getPdfValues();

    int newNumBins = static_cast<int>(std::ceil(originalPdf.size() / static_cast<double>(factor)));
    std::vector<double> newPdf(newNumBins, 0.0);

    for (size_t i = 0; i < originalPdf.size(); ++i) {
        newPdf[i / factor] += originalPdf[i];
    }

    return DeltaQ(targetBinWidth, newPdf, true);
}

DeltaQ convolve(const DeltaQ &lhs, const DeltaQ &rhs)
{
    double commonBinWidth = std::max(lhs.getBinWidth(), rhs.getBinWidth());

    if (lhs == DeltaQ()) {
        return rhs;
    }
    if (rhs == DeltaQ()) {
        return lhs;
    }

    DeltaQ lhsRebinned = rebin(lhs, commonBinWidth);
    DeltaQ rhsRebinned = rebin(rhs, commonBinWidth);

    const auto &lhsPdf = lhsRebinned.getPdfValues();
    const auto &rhsPdf = rhsRebinned.getPdfValues();

    int resultSize = lhsPdf.size() + rhsPdf.size() - 1;
    std::vector<double> resultPdf(resultSize, 0.0);

    for (size_t i = 0; i < lhsPdf.size(); ++i) {
        for (size_t j = 0; j < rhsPdf.size(); ++j) {
            resultPdf[i + j] += lhsPdf[i] * rhsPdf[j];
        }
    }

    return DeltaQ(commonBinWidth, resultPdf, true);
}

// Inspired by https://github.com/jeremyfix/FFTConvolution/blob/master/Convolution/src/convolution_fftw.h
DeltaQ convolveFFT(const DeltaQ &lhs, const DeltaQ &rhs)
{
    if (lhs == DeltaQ()) {
        return rhs;
    }
    if (rhs == DeltaQ()) {
        return lhs;
    }

    // Find a common bin width and rebin accordingly
    double commonBinWidth = std::max(lhs.getBinWidth(), rhs.getBinWidth());

    DeltaQ lhsRebinned = rebin(lhs, commonBinWidth);
    DeltaQ rhsRebinned = rebin(rhs, commonBinWidth);

    const auto &lhsPdf = lhsRebinned.getPdfValues();
    const auto &rhsPdf = rhsRebinned.getPdfValues();

    // Find the power of 2 nearest to the convolution size
    size_t lhsSize = lhsPdf.size();
    size_t rhsSize = rhsPdf.size();
    size_t convSize = lhsSize + rhsSize - 1;
    size_t fftSize = 1;
    while (fftSize < convSize)
        fftSize <<= 1;

    // Pad pdf with zeroes until end of PDF
    std::vector<double> lhsPadded(fftSize, 0.0);
    std::vector<double> rhsPadded(fftSize, 0.0);
    std::copy(lhsPdf.begin(), lhsPdf.end(), lhsPadded.begin());
    std::copy(rhsPdf.begin(), rhsPdf.end(), rhsPadded.begin());
    static std::mutex fftw_mutex;
    {
        std::lock_guard<std::mutex> lock(fftw_mutex);
        fftw_complex *lhsFreq = (fftw_complex *)fftw_malloc(sizeof(fftw_complex) * (fftSize / 2 + 1));
        fftw_complex *rhsFreq = (fftw_complex *)fftw_malloc(sizeof(fftw_complex) * (fftSize / 2 + 1));
        double *lhsTime = lhsPadded.data();
        double *rhsTime = rhsPadded.data();

        // Transform real input to complex output
        fftw_plan planLhs = fftw_plan_dft_r2c_1d(fftSize, lhsTime, lhsFreq, FFTW_ESTIMATE);
        fftw_plan planRhs = fftw_plan_dft_r2c_1d(fftSize, rhsTime, rhsFreq, FFTW_ESTIMATE);
        fftw_execute(planLhs);
        fftw_execute(planRhs);

        // Do complex multiplication, r: ac - bd i : ad + bc
        fftw_complex *resultFreq = (fftw_complex *)fftw_malloc(sizeof(fftw_complex) * (fftSize / 2 + 1));
        for (size_t i = 0; i < fftSize / 2 + 1; ++i) {
            double a = lhsFreq[i][0], b = lhsFreq[i][1];
            double c = rhsFreq[i][0], d = rhsFreq[i][1];
            resultFreq[i][0] = a * c - b * d;
            resultFreq[i][1] = a * d + b * c;
        }

        // Invert from complex plane to real plane
        std::vector<double> resultTime(fftSize);
        fftw_plan planInv = fftw_plan_dft_c2r_1d(fftSize, resultFreq, resultTime.data(), FFTW_ESTIMATE);
        fftw_execute(planInv);

        for (auto &val : resultTime)
            val /= fftSize;

        resultTime.resize(convSize);

        fftw_destroy_plan(planLhs);
        fftw_destroy_plan(planRhs);
        fftw_destroy_plan(planInv);
        fftw_free(lhsFreq);
        fftw_free(rhsFreq);
        fftw_free(resultFreq);
        return {lhsRebinned.getBinWidth(), resultTime, true};
    }
}

DeltaQ convolveN(const std::vector<DeltaQ> &deltaQs)
{
    if (deltaQs.empty())
        return DeltaQ();

    DeltaQ result = deltaQs[0];
    for (size_t i = 1; i < deltaQs.size(); ++i) {
        result = convolveFFT(result, deltaQs[i]);
    }
    return result;
}

DeltaQ probabilisticChoice(const std::vector<double> &probabilities, const std::vector<DeltaQ> &deltaQs)
{
    std::vector<DeltaQ> nonEmpty;
    std::vector<double> effectiveProbs;
    double commonBinWidth = 0.0;

    for (size_t i = 0; i < deltaQs.size(); ++i) {
        if (deltaQs[i] == DeltaQ()) {
            continue;
        }
        nonEmpty.push_back(deltaQs[i]);
        effectiveProbs.push_back(probabilities[i]);
        commonBinWidth = std::max(commonBinWidth, deltaQs[i].getBinWidth());
    }

    if (nonEmpty.empty()) {
        return DeltaQ();
    }

    for (auto &dq : nonEmpty) {
        dq = rebin(dq, commonBinWidth);
    }

    std::vector<DeltaQ> scaledDeltaQs;
    for (size_t i = 0; i < nonEmpty.size(); ++i) {
        scaledDeltaQs.push_back(nonEmpty[i] * effectiveProbs[i]);
    }

    DeltaQ result = scaledDeltaQs[0];
    for (size_t i = 1; i < scaledDeltaQs.size(); ++i) {
        result = result + scaledDeltaQs[i];
    }

    return result;
}

DeltaQ firstToFinish(const std::vector<DeltaQ> &deltaQs)
{
    std::vector<DeltaQ> nonEmpty;
    double commonBinWidth = 0.0;

    for (const auto &dq : deltaQs) {
        if (dq == DeltaQ()) {
            continue;
        }
        nonEmpty.push_back(dq);
        commonBinWidth = std::max(commonBinWidth, dq.getBinWidth());
    }

    if (nonEmpty.empty()) {
        return DeltaQ();
    }

    for (auto &dq : nonEmpty) {
        dq = rebin(dq, commonBinWidth);
    }

    const int largestSize = chooseLongestDeltaQSize(nonEmpty);
    std::vector<double> resultingCdf;

    for (int i = 0; i < largestSize; ++i) {
        double sumAtI = 0;
        double productAtI = 1;
        for (const auto &dq : nonEmpty) {
            const double cdfAtI = dq.cdfAt(i);
            sumAtI += cdfAtI;
            productAtI *= cdfAtI;
        }
        resultingCdf.push_back(sumAtI - productAtI);
    }
    return {commonBinWidth, resultingCdf, false};
}

DeltaQ allToFinish(const std::vector<DeltaQ> &deltaQs)
{
    std::vector<DeltaQ> nonEmpty;
    double commonBinWidth = 0.0;

    for (const auto &dq : deltaQs) {
        if (dq == DeltaQ()) {
            continue;
        }
        nonEmpty.push_back(dq);
        commonBinWidth = std::max(commonBinWidth, dq.getBinWidth());
    }

    if (nonEmpty.empty()) {
        return DeltaQ();
    }

    for (auto &dq : nonEmpty) {
        dq = rebin(dq, commonBinWidth);
    }

    DeltaQ result = nonEmpty[0];
    for (size_t i = 1; i < nonEmpty.size(); ++i) {
        result = result * nonEmpty[i];
    }

    return result;
}

int chooseLongestDeltaQSize(const std::vector<DeltaQ> &deltaQs)
{
    int highestSize = 0;
    for (const DeltaQ &deltaQ : deltaQs) {
        if (deltaQ.getBins() > highestSize) {
            highestSize = deltaQ.getBins();
        }
    }
    return highestSize;
}
