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
    std::vector<double> pdfValues;
    std::vector<double> cdfValues;
    int size {};

    /**
     * Calculate PDF and CDF values given samples from an outcome
     */
    void calculateDeltaQ(std::vector<double> &outcomeSamples);

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
    DeltaQ(double binWidth, const std::vector<double> &values, bool isPdf);
    DeltaQ(double binWidth, const std::vector<double> outcomeSamples);
    /**
     * Processes outcome samples to generate PDF and CDF.
     */
    void processSamples(std::vector<double> &outcomeSamples);

    /**
     * Getters
     */
    [[nodiscard]] const std::vector<double> &getPdfValues() const;
    [[nodiscard]] const std::vector<double> &getCdfValues() const;
    [[nodiscard]] double getBinWidth() const;
    [[nodiscard]] int getSize() const;
    [[nodiscard]] double pdfAt(int x) const;
    [[nodiscard]] double cdfAt(int x) const;
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
