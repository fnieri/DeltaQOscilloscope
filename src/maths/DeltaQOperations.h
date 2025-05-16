#pragma once

#include "DeltaQ.h"

/**
 * Perform discrete convolution between two DeltaQs
 */
DeltaQ convolve(const DeltaQ &lhs, const DeltaQ &rhs);
/**
 * Perform Fast Fourier Transform on two DeltaQs
 */
DeltaQ convolveFFT(const DeltaQ &lhs, const DeltaQ &rhs);
DeltaQ convolveN(const std::vector<DeltaQ> &deltaQs);

/**
 * Assume two independent outcomes with the same start event
 * All-to-finish outcome occurs when both end events occur
 * All-to-finish is defined as ΔQ_{LTF(A,B)} ΔQ_A * ΔQ_B
 */
DeltaQ allToFinish(const std::vector<DeltaQ> &deltaQs);

/**
 * Assume two independent outcomes with the same start event
 * First-to-finish outcome occurs when at least one end event occurs
 * We compute the probability that there are zero end events
 * First-to-finish is defined as
 * ΔQ_{FTF(A,B)} = ΔQ_A + ΔQ_B – ΔQ_A * ΔQ_B
 */
DeltaQ firstToFinish(const std::vector<DeltaQ> &deltaQs);

/**
 *  Assume there are two possible outcomes OA and OB and
 * exactly one outcome is chosen during each occurrence of a start event
 * O_A occurs with probability p/(p+q)
 * O_B occurs with probability q/(p+q)
 * Therefore:
 * ΔQ_{PC(A,B)} = p/(p+q) ΔQ_A + q/(p+q) ΔQ_B
 */
DeltaQ probabilisticChoice(const std::vector<double> &probabilities, const std::vector<DeltaQ> &deltaQs);

DeltaQ rebin(const DeltaQ &source, double targetBinWidth);

/**
 * Choose the highest size from a list of DeltaQs
 */
int chooseLongestDeltaQSize(const std::vector<DeltaQ> &deltaQs);
