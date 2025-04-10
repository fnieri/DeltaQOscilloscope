

#include "DeltaQ.h"

#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>
DeltaQ::DeltaQ(const double binWidth)
    : binWidth(binWidth)
    , bins(0)
{
}

DeltaQ::DeltaQ(const double binWidth, const std::vector<double> &values, const bool isPdf)
    : binWidth(binWidth)
    , bins(values.size()) // Values is binned data
{
    if (isPdf) {
        pdfValues = values;
        calculateCDF();
    } else {
        cdfValues = values;
        calculatePDF();
    }
}
DeltaQ::DeltaQ(double binWidth, std::vector<Sample> samples)
    : binWidth(binWidth)
    , bins {0} // FIXME magic number
    , samples(samples)
{
    calculateDeltaQ(samples);
}
void DeltaQ::calculateDeltaQ(std::vector<Sample> &outcomeSamples)
{
    if (outcomeSamples.empty() || binWidth <= 0)
        return;

    const int numBins = 50;
    std::vector<int> histogram = std::vector<int>(numBins, 0);
    cumulativeHistogram = std::vector<int>(numBins, 0);
    totalSamples = outcomeSamples.size();
    long long successfulSamples = 0;

    for (const auto &sample : outcomeSamples) {
        if (sample.status != Status::SUCCESS) {
            continue; // Exclude failed samples from histogram but count them
        }

        successfulSamples++;
        double elapsed = sample.elapsedTime;

        if (elapsed < 0 || std::isnan(elapsed) || std::isinf(elapsed)) {
            std::cerr << "Warning: Invalid sample value: " << elapsed << std::endl;
            continue;
        }

        int bin = std::floor(elapsed / binWidth);
        if (bin < 0) {
            std::cerr << "Warning: Negative bin value: " << bin << std::endl;
            continue;
        }
        if (bin >= numBins) {
            bin = numBins - 1;
        }

        histogram[bin]++;
    }
    bins = numBins;
    // Calculate PDF
    pdfValues.reserve(numBins);
    for (const double &binValue : histogram) {
        pdfValues.push_back(binValue / totalSamples);
    }

    cumulativeHistogram[0] = histogram[0];
    for (int i = 1; i < numBins; ++i) {
        cumulativeHistogram[i] = cumulativeHistogram[i - 1] + histogram[i];
    }
    calculateCDF();
}

void DeltaQ::calculateCDF()
{
    cdfValues.clear();
    cdfValues.reserve(bins);
    double cumulativeSum = 0;
    for (const double &pdfValue : pdfValues) {
        cumulativeSum += pdfValue;
        cdfValues.push_back(cumulativeSum);
    }
}

void DeltaQ::calculatePDF()
{
    pdfValues.clear();

    double previous = 0;
    for (const double &cdfValue : cdfValues) {
        pdfValues.push_back(cdfValue - previous);
        previous = cdfValue;
    }
}

const std::vector<double> &DeltaQ::getPdfValues() const
{
    return pdfValues;
}

const std::vector<double> &DeltaQ::getCdfValues() const
{
    return cdfValues;
}

const std::vector<int> &DeltaQ::getCumulativeHistogram() const
{
    return cumulativeHistogram;
}

const unsigned int DeltaQ::getTotalSamples() const
{
    return totalSamples;
}

void DeltaQ::setBinWidth(double newWidth)
{
    binWidth = newWidth;
}

double DeltaQ::getBinWidth() const
{
    return binWidth;
}

int DeltaQ::getSize() const
{
    return bins;
}

double DeltaQ::pdfAt(int x) const
{
    if (x >= bins) {
        return 0.0;
    }
    return pdfValues.at(x);
}

double DeltaQ::cdfAt(int x) const
{
    if (x >= bins) {
        return cdfValues.at(bins);
    }
    return cdfValues.at(x);
}

bool DeltaQ::operator<(const DeltaQ &other) const
{
    return this->bins < other.bins;
}

bool DeltaQ::operator>(const DeltaQ &other) const
{
    return this->bins > other.bins;
}

bool DeltaQ::operator==(const DeltaQ &deltaQ) const
{
    return pdfValues == deltaQ.getPdfValues();
}

DeltaQ operator*(const DeltaQ &deltaQ, double constant)
{
    DeltaQ result(deltaQ.binWidth);
    result.bins = deltaQ.bins;

    result.pdfValues = deltaQ.pdfValues;
    std::transform(result.pdfValues.begin(), result.pdfValues.end(), result.pdfValues.begin(), [constant](double value) { return value * constant; });

    result.cdfValues = deltaQ.cdfValues;
    std::transform(result.cdfValues.begin(), result.cdfValues.end(), result.cdfValues.begin(), [constant](double value) { return value * constant; });

    return result;
}

template <typename BinaryOperation>
DeltaQ applyBinaryOperation(const DeltaQ &lhs, const DeltaQ &rhs, BinaryOperation op)
{
    const DeltaQ &highestDeltaQ = (lhs > rhs) ? lhs : rhs;
    const DeltaQ &otherDeltaQ = (lhs > rhs) ? rhs : lhs;

    std::vector<double> resultingCdf;
    resultingCdf.reserve(highestDeltaQ.getSize());

    for (size_t i = 0; i < highestDeltaQ.getSize(); i++) {
        double result = op(highestDeltaQ.cdfAt(i), otherDeltaQ.cdfAt(i));
        resultingCdf.push_back(result);
    }

    return {highestDeltaQ.getBinWidth(), resultingCdf, false};
}

DeltaQ operator*(double constant, const DeltaQ &deltaQ)
{
    return deltaQ * constant;
}

DeltaQ operator+(const DeltaQ &lhs, const DeltaQ &rhs)
{
    return applyBinaryOperation(lhs, rhs, std::plus<>());
}

DeltaQ operator-(const DeltaQ &lhs, const DeltaQ &rhs)
{
    return applyBinaryOperation(lhs, rhs, std::minus<>());
}

DeltaQ operator*(const DeltaQ &lhs, const DeltaQ &rhs)
{
    return applyBinaryOperation(lhs, rhs, std::multiplies<>());
}

std::string DeltaQ::toString() const
{
    std::ostringstream oss;
    oss << "<";

    // Iterate through CDF values to construct the string
    for (size_t i = 0; i < cdfValues.size(); ++i) {
        const double bin = i * binWidth;
        oss << "(" << std::fixed << std::setprecision(3) << bin << ", " << std::setprecision(6) << cdfValues[i] << ")";
        if (i < cdfValues.size() - 1) {
            oss << ", ";
        }
    }

    oss << ">";
    return oss.str();
}
