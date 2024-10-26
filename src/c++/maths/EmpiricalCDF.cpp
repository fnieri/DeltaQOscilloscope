/**
 * @author: Francesco Nieri
 * @date 26/10/2024
 * Class representing an Empirical CDF
 */

#include <boost/math/distributions/empirical_cumulative_distribution_function.hpp>

#include <memory>
#include "Outcome.h"
#include <numeric>
#include <cmath>

#define NUM_BINS 100

struct BinData {
    double binCenter;
    double ecdfValue;
    double pdfValue;
};


struct EmpiricalCDF {   
    std::vector<double> samples; 
    std::vector<BinData> ecdfPdfData;

    double min = 0;
    double max = 0;
    
    double mean = 0.0;
    double variance = 0.0;

    void computeStatistics();

    void calculateMinMax();

    void calculateMean();

    void calculateVariance();

    void calculateEcdfAndPdf();

    void operator*(const int& constant);

    std::vector<BinData> operator+(const EmpiricalCDF& rhs) const;
}; 
        

void EmpiricalCDF::computeStatistics() {

    calculateMinMax();
    calculateMean();
    // Variance needs to be calculated after mean as it relies on the latter
    calculateVariance();

    calculateEcdfAndPdf();
}

void EmpiricalCDF::calculateMean() {
    double sum = std::accumulate(samples.begin(), samples.end(), 0.0);
    mean = sum / samples.size();
}

void EmpiricalCDF::calculateVariance() {
    double variance_sum = 0.0;
    for (double sample : samples) {
        variance_sum += std::pow(sample - mean, 2);
    }
    variance = variance_sum / samples.size();
}

void EmpiricalCDF::calculateMinMax() {
    min = *std::min_element(samples.begin(), samples.end());
    max = *std::max_element(samples.begin(), samples.end());
}

void EmpiricalCDF::calculateEcdfAndPdf() {
    //TODO replace this, we will need fixed min and max values
    double binWidth = (max - min) / NUM_BINS;
    
    std::vector<int> binCounts(NUM_BINS, 0);
    std::vector<BinData> EcdfPdfData;

    for (double sample : samples) {
        int binIndex = static_cast<int>((sample - min) / binWidth);
        binIndex = std::min(binIndex, NUM_BINS - 1);  
        binCounts[binIndex]++;
    }

    int sampleCount = samples.size();
    double cumulativeProbability = 0.0;

    for (int i = 0; i < NUM_BINS; ++i) {
        double binCenter = min + ( i + 0.5) * binWidth;
        double pdfValue = static_cast<double>(binCounts[i]) / (sampleCount * binWidth);
        cumulativeProbability += static_cast<double>(binCounts[i]) / sampleCount;

        EcdfPdfData.push_back({binCenter, cumulativeProbability, pdfValue});
    }
    
    ecdfPdfData = EcdfPdfData;

}

void EmpiricalCDF::operator*(const int& constant) {
    for (BinData binData: ecdfPdfData) {
        binData.ecdfValue *= constant;
        binData.pdfValue *= constant;
    }
}

std::vector<BinData> EmpiricalCDF::operator+(const EmpiricalCDF& rhs) const {
    std::vector<BinData> finalBinData;

    // Assuming both objects have the same bin setup
    for (int i = 0; i < NUM_BINS; i++) {
        BinData summedBin;
        summedBin.binCenter = ecdfPdfData[i].binCenter;
        summedBin.ecdfValue = ecdfPdfData[i].ecdfValue + rhs.ecdfPdfData[i].ecdfValue;
        summedBin.pdfValue = ecdfPdfData[i].pdfValue + rhs.ecdfPdfData[i].pdfValue;
        finalBinData.push_back(summedBin);
    }

    return finalBinData;
}

std::vector<BinData> convolve(const EmpiricalCDF& lhs, const EmpiricalCDF& rhs) {
    return;
}