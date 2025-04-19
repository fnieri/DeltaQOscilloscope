#include "../maths/DeltaQ.h"
#include "../maths/DeltaQOperations.h"
#include <assert.h>
#include <cmath>
#include <iostream>
// Test cases

// Assumes rebinToCoarser and convolve definitions from before (same as last message)

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

int main()
{
    test_case_1();
    test_case_2();
    test_case_3();
    test_case_4();
    test_case_5();
    return 0;
}
