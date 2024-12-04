

#include "DeltaQ.h"


#include <algorithm>
#include <map>
#include <cmath>
#include <functional>
#include <iomanip>
#include <iostream>

DeltaQ::DeltaQ(const double binWidth) : binWidth(binWidth), size(0) {}

DeltaQ::DeltaQ(const double binWidth, const std::vector<double>& values, const bool isPdf) :
    binWidth(binWidth),
    size(values.size())
    {
        if (isPdf) {
            pdfValues = values;
            calculateCDF();
        }
        else {
            cdfValues = values;
            calculatePDF();
        }
    }

DeltaQ::DeltaQ(const double binWidth, std::vector<double> outcomeSamples) :
    binWidth{binWidth}
    {
        processSamples(outcomeSamples);
    }

    void DeltaQ::calculateDeltaQ(std::vector<double>& outcomeSamples) {
        if (outcomeSamples.empty()) return;
        std::sort(outcomeSamples.begin(), outcomeSamples.end());

        const int numBins = 10;
        std::vector<double> histogram(numBins);


        // Add samples to corresponding bins
        for (const double& sample : outcomeSamples) {
            const int bin = std::floor(sample / binWidth);
            histogram[bin]++;
        }

        // Calculate discrete PDF
        const auto outcomesSize = static_cast<double>(outcomeSamples.size());
        for (const double& binValue : histogram) {
            pdfValues.push_back(binValue / outcomesSize);
        }

        calculateCDF();
        size = pdfValues.size();
    }

    void DeltaQ::calculateCDF() {
        cdfValues.clear();
        double cumulativeSum = 0;
        for (const double& pdfValue : pdfValues) {
            cumulativeSum += pdfValue;
            cdfValues.push_back(cumulativeSum);
        }
    }

void DeltaQ::calculatePDF() {
    pdfValues.clear();

    double previous = 0;
    for (const double& cdfValue: cdfValues) {
        pdfValues.push_back(cdfValue - previous);
        previous = cdfValue;
    }
}

void DeltaQ::processSamples(std::vector<double>& outcomeSamples) {
    pdfValues.clear();
    cdfValues.clear();
    calculateDeltaQ(outcomeSamples);
}

const std::vector<double>& DeltaQ::getPdfValues() const {
    return pdfValues;
}

const std::vector<double>& DeltaQ::getCdfValues() const {
    return cdfValues;
}

double DeltaQ::getBinWidth() const {
    return binWidth;
}

int DeltaQ::getSize() const {
    return size;
}

double DeltaQ::pdfAt(int x) const {
    if (x >= size) {
        return 0.0;
    }
    return pdfValues.at(x);
}

double DeltaQ::cdfAt(int x) const {
    if (x >= size) {
        return 1.0;
    }
    return cdfValues.at(x);
}

bool DeltaQ::operator<(const DeltaQ& other) const {
    return this->size < other.size;
}

bool DeltaQ::operator>(const DeltaQ& other) const {
    return this->size > other.size;
}

bool DeltaQ::operator==(const DeltaQ &deltaQ) const {
    return pdfValues == deltaQ.getPdfValues();
}


DeltaQ operator*(const DeltaQ& deltaQ, double constant) {
    DeltaQ result(deltaQ.binWidth);
    result.size = deltaQ.size;

    result.pdfValues = deltaQ.pdfValues;
    std::transform(result.pdfValues.begin(), result.pdfValues.end(), result.pdfValues.begin(),
                [constant](double value) { return value * constant; });

    result.cdfValues = deltaQ.cdfValues;
    std::transform(result.cdfValues.begin(), result.cdfValues.end(), result.cdfValues.begin(),
                [constant](double value) { return value * constant; });

    return result;
}


template <typename BinaryOperation>
DeltaQ applyBinaryOperation(const DeltaQ& lhs, const DeltaQ& rhs, BinaryOperation op) {
    const DeltaQ& highestDeltaQ = (lhs > rhs) ? lhs : rhs;
    const DeltaQ& otherDeltaQ = (lhs > rhs) ? rhs : lhs;

    std::vector<double> resultingCdf;
    resultingCdf.reserve(highestDeltaQ.getSize());

    for (size_t i = 0; i < highestDeltaQ.getSize(); i++) {
        double result = op(highestDeltaQ.cdfAt(i), otherDeltaQ.cdfAt(i));
        resultingCdf.push_back(result);
    }

    return {highestDeltaQ.getBinWidth(), resultingCdf, false};
}

DeltaQ operator*(double constant, const DeltaQ& deltaQ) {
    return deltaQ * constant;
}


DeltaQ operator+(const DeltaQ& lhs, const DeltaQ& rhs) {
    return applyBinaryOperation(lhs, rhs, std::plus<>());
}

DeltaQ operator-(const DeltaQ& lhs, const DeltaQ& rhs) {
    return applyBinaryOperation(lhs, rhs, std::minus<>());
}

DeltaQ operator*(const DeltaQ& lhs, const DeltaQ& rhs) {
    return applyBinaryOperation(lhs, rhs, std::multiplies<>());
}

std::string DeltaQ::toString() const {
    std::ostringstream oss;
    oss << "<";

    // Iterate through CDF values to construct the string
    for (size_t i = 0; i < cdfValues.size(); ++i) {
        const double bin = i * binWidth;
        oss << "(" << std::fixed << std::setprecision(3) << bin << ", "
            << std::setprecision(6) << pdfValues[i] << ")";
        if (i < cdfValues.size() - 1) {
            oss << ", ";
        }
    }

    oss << ">";
    return oss.str();
}