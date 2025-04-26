#include "../maths/DeltaQ.h"
#include "../maths/DeltaQOperations.h"
#include <assert.h>
#include <cmath>
#include <iostream>
#include <numeric>
// Test cases
//

#define ASSERT_TRUE(expr)                                                                                                                                      \
    if (!(expr)) {                                                                                                                                             \
        std::cerr << "Assertion failed at " << __LINE__ << ": " << #expr << std::endl;                                                                         \
        std::exit(1);                                                                                                                                          \
    }

#define ASSERT_NEAR(a, b, tol)                                                                                                                                 \
    if (std::fabs((a) - (b)) > tol) {                                                                                                                          \
        std::cerr << "Assertion failed at " << __LINE__ << ": " << #a << " ≈ " << #b << " (" << a << " vs " << b << ")" << std::endl;                          \
        std::exit(1);                                                                                                                                          \
    }

void printPdf(const DeltaQ &dq, const std::string &label)
{
    std::cout << label << " (binWidth = " << dq.getBinWidth() << "):\n[ ";
    for (const auto &v : dq.getPdfValues())
        std::cout << v << " ";
    std::cout << "]\n";
}

void test_case_1()
{
    std::cout << "\n--- Test Case 1: Same Bin Width, Simple Pulse ---\n";
    DeltaQ a(0.001, {0, 1, 0}, true);
    DeltaQ b(0.001, {0.5, 0.5}, true);

    DeltaQ result = convolve(a, b);
    printPdf(a, "A");
    printPdf(b, "B");
    printPdf(result, "A * B");
}

void test_case_2()
{
    std::cout << "\n--- Test Case 2: Different Bin Widths (A finer) ---\n";
    DeltaQ a(0.001, {0.3, 0.3, 0.4}, true);
    DeltaQ b(0.002, {0.7, 0.3}, true);

    DeltaQ result = convolve(a, b);
    printPdf(a, "A");
    printPdf(b, "B");
    printPdf(result, "A * B");
}

void test_case_3()
{
    std::cout << "\n--- Test Case 3: Different Bin Widths (B finer) ---\n";
    DeltaQ a(0.002, {0.2, 0.8}, true);
    DeltaQ b(0.001, {0.25, 0.5, 0.25}, true);

    DeltaQ result = convolve(a, b);
    printPdf(a, "A");
    printPdf(b, "B");
    printPdf(result, "A * B");
}

void test_case_4()
{
    std::cout << "\n--- Test Case 4: Mass < 1 (Failures) ---\n";
    DeltaQ a(0.001, {0.2, 0.3}, true); // Total = 0.5
    DeltaQ b(0.002, {0.4, 0.1}, true); // Total = 0.5

    DeltaQ result = convolve(a, b);
    printPdf(a, "A");
    printPdf(b, "B");
    printPdf(result, "A * B (should have total mass = 0.25)");
}

void test_case_5()
{
    std::cout << "\n--- Test Case 5: Uniform PDFs ---\n";
    DeltaQ a(0.002, {0.25, 0.25, 0.25, 0.25}, true); // Total = 1.0
    DeltaQ b(0.002, {0.5, 0.5}, true); // Total = 1.0

    DeltaQ result = convolve(a, b);
    printPdf(a, "A");
    printPdf(b, "B");
    printPdf(result, "A * B");
}

bool pdfsAlmostEqual(const std::vector<double> &a, const std::vector<double> &b, double tol = 1e-6)
{
    if (a.size() != b.size())
        return false;
    for (size_t i = 0; i < a.size(); ++i)
        if (std::fabs(a[i] - b[i]) > tol)
            return false;
    return true;
}
void test_DeltaConvolution()
{
    std::vector<double> delta = {1.0}; // delta function
    std::vector<double> pdf = {0.1, 0.2, 0.4, 0.2, 0.1};

    DeltaQ lhs(1.0, delta, true);
    std::cout << "lhs num bins " << lhs.getSize() << "\n";
    DeltaQ rhs(1.0, pdf, true);
    std::cout << "rhs num bins " << rhs.getSize() << "\n";
    auto result = convolveFFT(lhs, rhs);

    ASSERT_TRUE(pdfsAlmostEqual(result.getPdfValues(), pdf));
    printPdf(result, "convolution FFT");
    std::cout << "✅ test_DeltaConvolution passed.\n";
}

void test_UniformConvolution()
{
    std::vector<double> uniform = {0.25, 0.25, 0.25, 0.25};
    DeltaQ a(1.0, uniform, true);
    DeltaQ b(1.0, uniform, true);

    auto result = convolveFFT(a, b);
    const auto &out = result.getPdfValues();

    // Expect triangle shape: 0.0625, 0.125, 0.1875, 0.25, ..., 0.0625
    std::vector<double> expected = {0.0625, 0.125, 0.1875, 0.25, 0.1875, 0.125, 0.0625};

    ASSERT_TRUE(pdfsAlmostEqual(out, expected, 1e-6));

    std::cout << "✅ test_UniformConvolution passed.\n";
}

void test_Symmetry()
{
    std::vector<double> pdf = {0.3, 0.4, 0.3};
    DeltaQ a(1.0, pdf, true);
    DeltaQ b(1.0, pdf, true);

    auto resultAB = convolveFFT(a, b);
    auto resultBA = convolveFFT(b, a);

    ASSERT_TRUE(pdfsAlmostEqual(resultAB.getPdfValues(), resultBA.getPdfValues()));
    std::cout << "✅ test_Symmetry passed.\n";
}

void test_Normalization()
{
    std::vector<double> a = {0.2, 0.3, 0.5};
    std::vector<double> b = {0.1, 0.4, 0.5};

    DeltaQ lhs(1.0, a, true);
    DeltaQ rhs(1.0, b, true);

    auto result = convolveFFT(lhs, rhs);
    double sum = std::accumulate(result.getPdfValues().begin(), result.getPdfValues().end(), 0.0);

    ASSERT_NEAR(sum, 1.0, 1e-6);
    std::cout << "✅ test_Normalization passed.\n";
}

void test_LargerConvolution()
{
    std::vector<double> a(100, 1.0 / 100); // flat
    std::vector<double> b(50, 1.0 / 50); // flat

    DeltaQ lhs(1.0, a, true);
    DeltaQ rhs(1.0, b, true);

    auto result = convolveFFT(lhs, rhs);
    double sum = std::accumulate(result.getPdfValues().begin(), result.getPdfValues().end(), 0.0);

    ASSERT_NEAR(sum, 1.0, 1e-6);
    std::cout << "✅ test_LargerConvolution passed.\n";
}

int main()
{
    test_case_1();
    test_case_2();
    test_case_3();
    test_case_4();
    test_case_5();

    test_DeltaConvolution();
    test_UniformConvolution();
    test_Symmetry();
    test_Normalization();
    test_LargerConvolution();
    return 0;
}
