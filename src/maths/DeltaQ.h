/**
 * @author: Francesco Nieri
 * @date 26/10/2024
 * Class representing a Delta Q
 */

#ifndef DELTAQ_H
#define DELTAQ_H

#pragma once

#include <ostream>
#include <vector>

class DeltaQ
{

    double binWidth;
    std::vector<long double> pdfValues;
    std::vector<long double> cdfValues;
    int size {};

    /**
     * Calculate PDF and CDF values given samples from an outcome
     */
    void calculateDeltaQ(std::vector<long double> &outcomeSamples);
    void calculateDeltaQ(std::vector<long double> &outcomeSamples, int nBins);
    /**
     * Calculate CDF given PDF values
     */
    void calculateCDF();
    /**
     * Calculate PDF from a given CDF
     */
    void calculatePDF();

public:
    DeltaQ() = default;
    DeltaQ(double binWidth);
    DeltaQ(double binWidth, const std::vector<long double> &values, bool isPdf);
    DeltaQ(double binWidth, const std::vector<long double> outcomeSamples);
    DeltaQ(double binWidth, const std::vector<long double> outcomeSamples, int nBins);
    /**
     * Processes outcome samples to generate PDF and CDF.
     */
    void processSamples(std::vector<long double> &outcomeSamples);
    void processSamples(std::vector<long double> &outcomeSamples, int nBins);

    /**
     * Getters
     */
    [[nodiscard]] const std::vector<long double> &getPdfValues() const;
    [[nodiscard]] const std::vector<long double> &getCdfValues() const;
    [[nodiscard]] double getBinWidth() const;
    [[nodiscard]] int getSize() const;
    [[nodiscard]] double pdfAt(int x) const;
    [[nodiscard]] double cdfAt(int x) const;

    void setBinWidth(double newWidth);
    /**
     * Operator Overloads
     */
    friend DeltaQ operator*(const DeltaQ &deltaQ, double constant);
    friend DeltaQ operator*(double constant, const DeltaQ &deltaQ);
    friend DeltaQ operator*(const DeltaQ &lhs, const DeltaQ &rhs);
    friend DeltaQ operator+(const DeltaQ &lhs, const DeltaQ &rhs);
    friend DeltaQ operator-(const DeltaQ &lhs, const DeltaQ &rhs);

    friend std::ostream &operator<<(std::ostream &os, const DeltaQ &deltaQ);

    /**
     * Comparison Operators
     */
    bool operator<(const DeltaQ &other) const;
    bool operator>(const DeltaQ &other) const;

    bool operator==(const DeltaQ &deltaQ) const;
    [[nodiscard]] std::string toString() const;
};

#endif // DELTAQ_H
