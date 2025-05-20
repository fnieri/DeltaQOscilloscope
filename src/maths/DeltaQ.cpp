

#include "DeltaQ.h"

#include <algorithm>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>

#include <chrono>
#include <iostream>
DeltaQ::DeltaQ(const double binWidth)
    : binWidth(binWidth)
    , bins(0)
    , qta()
{
}

DeltaQ::DeltaQ(const double binWidth, const std::vector<double> &values, const bool isPdf)
    : binWidth(binWidth)
    , bins(values.size()) // Values is binned data
    , qta()
{
    if (isPdf) {
        pdfValues = values;
        calculateCDF();
    } else {
        cdfValues = values;
        calculatePDF();
    }
}
DeltaQ::DeltaQ(double binWidth, std::vector<Sample> &samples)
    : binWidth(binWidth)
    , bins {50}
    , qta()
{
    calculateDeltaQ(samples);
}

DeltaQ::DeltaQ(double binWidth, std::vector<Sample> &samples, int bins)
    : binWidth(binWidth)
    , bins(bins)
    , qta()
{
    calculateDeltaQ(samples);
}

void DeltaQ::calculateDeltaQ(std::vector<Sample> &outcomeSamples)
{

    if (outcomeSamples.empty() || binWidth <= 0) {
        bins = 0;
        return;
    }
    std::vector<double> histogram(bins, 0.0);
    totalSamples = outcomeSamples.size();
    long long successfulSamples = 0;

    std::sort(outcomeSamples.begin(), outcomeSamples.end(), [](const Sample &a, const Sample &b) { return a.elapsedTime < b.elapsedTime; });

    for (const auto &sample : outcomeSamples) {
        if (sample.status != Status::SUCCESS) {
            continue; // Exclude failed samples from histogram but count them
        }

        double elapsed = sample.elapsedTime;

        if (elapsed < 0 || std::isnan(elapsed) || std::isinf(elapsed)) {
            std::cerr << "Warning: Invalid sample value: " << elapsed << std::endl;
            continue;
        }
        double invBinWidth = 1.0 / binWidth;
        int bin = static_cast<int>(elapsed * invBinWidth);
        if (bin < 0) {
            std::cerr << "Warning: Negative bin value: " << bin << std::endl;
            continue;
        }
        if (bin >= bins) {
            if (elapsed > binWidth * bins) {
                continue;
            }
            bin = bins - 1;
        }

        successfulSamples++;
        histogram[bin] += 1.0;
    }
    // Calculate PDF
    for (double &val : histogram) {
        val /= totalSamples;
    }
    pdfValues = std::move(histogram);

    calculateCDF();
    calculateQuartiles(outcomeSamples);
}

void DeltaQ::calculateQuartiles(std::vector<Sample> &outcomeSamples)
{
    if (outcomeSamples.empty()) {
        return;
    }
    const size_t n = outcomeSamples.size();

    auto getElapsedAt = [&](size_t index) -> double { return outcomeSamples[index].elapsedTime; };

    auto getPercentile = [&](double p) -> double {
        double pos = p * (n - 1);
        auto idx = static_cast<size_t>(pos);
        double frac = pos - idx;

        if (idx + 1 < n) {
            // Linear interpolation between samples[idx] and samples[idx + 1]
            return getElapsedAt(idx) * (1.0 - frac) + getElapsedAt(idx + 1) * frac;
        }
        return getElapsedAt(idx);
    };
    qta = QTA::create(getPercentile(0.25), getPercentile(0.50), getPercentile(0.75), ((cdfAt(bins - 1)) > 1) ? 1 : cdfAt(bins - 1));
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

QTA DeltaQ::getQTA() const
{
    return qta;
}

const std::vector<double> &DeltaQ::getPdfValues() const
{
    return pdfValues;
}

const std::vector<double> &DeltaQ::getCdfValues() const
{
    return cdfValues;
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

int DeltaQ::getBins() const
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
        return cdfValues.at(bins - 1);
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
    resultingCdf.reserve(highestDeltaQ.getBins());

    for (size_t i = 0; i < highestDeltaQ.getBins(); i++) {
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
        oss << "(" << std::fixed << std::setprecision(7) << bin << ", " << std::setprecision(7) << cdfValues[i] << ")";
        if (i < cdfValues.size() - 1) {
            oss << ", ";
        }
    }

    oss << ">";
    return oss.str();
}
