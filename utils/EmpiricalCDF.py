import numpy as np
import math
from collections import defaultdict
import matplotlib.pyplot as plt
import numpy as np

class EmpiricalCDF:
    def __init__(self, samples, bin_width=0.1):
        """
        Initialize the EmpiricalCDF object with a list of samples.

        :param samples: List of samples (can include None for improper samples)
        """
        self.samples = samples
        self.cdf_map = defaultdict(int)
        self.ecdf = None
        self.normalized_cdf = []

        self.pdf_map = defaultdict(float)
        self.bin_width = bin_width

        self.ecdf_mass = 0
        self.ecdf_min = float('inf')
        self.ecdf_max = float('-inf')

        self.mean = 0
        self.variance = 0

        self._compute_statistics()

    def _compute_statistics(self):
        valid_samples = [s for s in self.samples if s is not None]

        for sample in valid_samples:
            self.cdf_map[sample] += 1

        sorted_cdf = sorted(self.cdf_map.items())

        if sorted_cdf:
            self.ecdf_min = sorted_cdf[0][0]
            self.ecdf_max = sorted_cdf[-1][0]
            self.ecdf_mass = sum(self.cdf_map.values()) / len(self)
        else:
            self.ecdf_min = float('inf')
            self.ecdf_max = float('-inf')

        # Normalize
        total_count = sum(self.cdf_map.values())
        cumulative_prob = 0
        for value, count in sorted_cdf:
            cumulative_prob += count / total_count
            self.normalized_cdf.append((value, cumulative_prob))

        self._compute_pdf(valid_samples)

        self.mean = np.mean(valid_samples) if valid_samples else 0
        self.variance = np.var(valid_samples) if len(valid_samples) > 1 else math.nan

    def _compute_pdf(self, valid_samples):
        """
        Estimate the PDF by binning the samples.
        """
        bins = np.arange(self.ecdf_min, self.ecdf_max + self.bin_width, self.bin_width)
        counts, _ = np.histogram(valid_samples, bins=bins)

        for i in range(len(counts)):
            bin_center = bins[i] + self.bin_width / 2
            self.pdf_map[bin_center] = counts[i] / (len(self) * self.bin_width)

    def get_plottable_ecdf(self):
        return zip(*self.normalized_cdf)

    def get_plottable_pdf(self):
        values = sorted(self.pdf_map.keys())
        pdf_probs = [self.pdf_map[v] for v in values]
        return values, pdf_probs

    def __str__(self):
        """
        Return a string representation of the important statistics of the EmpiricalCDF.
        """
        return (
            f"Number of samples: {len(self.samples)}\n"
            f"Mean: {self.mean:.10f}\n"
            f"Variance: {self.variance:.10f}\n"
            f"Min: {self.ecdf_min:.10f}\n"
            f"Max: {self.ecdf_max:.10f}\n"
            f"Total Probability Mass: {self.ecdf_mass:.4f}\n"
        )

    def __len__(self):
        return len(self.samples)

    def __mul__(self, other):
        if isinstance(other, (int, float)):
            return self._multiply_cdf_by_constant(other)

    def __rmul__(self, other):
        return self.__mul__(other)

    def __imul__(self, constant):
        if isinstance(constant, (int, float)):

            if constant < 0:
                raise ValueError("Constant must be non-negative.")

            scaled_samples = []
            for value, prob in self.normalized_cdf:
                scaled_prob = prob * constant
                scaled_samples.append((value, scaled_prob))

            self.samples = [s[0] for s in scaled_samples]
            self.normalized_cdf = scaled_samples
            self._compute_statistics()

        return self

    def __matmul__(self, other):
        """
        Convolution method, used when doing cdf1 @ cdf2
        """
        if not isinstance(other, EmpiricalCDF):
            raise ValueError("The right operand must be an instance of EmpiricalCDF.")

        convolved_samples = []

        for s1 in self.samples:
            for s2 in other.samples:
                convolved_samples.append(s1 + s2)
        return EmpiricalCDF(convolved_samples)

    def __rmatmul__(self, other):
        """
        Right side convolution, used when self is on right side
        """
        return self.__matmul__(other)

    def __imatmul__(self, other):
        """
        In place convolution, used when cdf1 @= cdf2
        """
        result = self.__matmul__(other)
        self.samples = result.samples
        self.normalized_cdf = result.normalized_cdf
        self.pdf_map = result.pdf_map
        self._compute_statistics()
        return self

    def _multiply_cdf_by_constant(self, constant):
        """
        Multiply the CDF by a constant.
        :param constant: The constant to multiply by.
        :return: A new EmpiricalCDF instance representing the scaled CDF.
        """
        if constant < 0:
            raise ValueError("Constant must be non-negative.")

        scaled_samples = []

        for value, prob in self.normalized_cdf:
            scaled_prob = prob * constant
            scaled_samples.append((value, scaled_prob))

        return EmpiricalCDF([s[0] for s in scaled_samples])


    def add_cdfs(self, other_ecdfs):
        """
        Add multiple empirical CDFs together.

        :param other_ecdfs: List of other EmpiricalCDF to be added
        :return: new EmpiricalCDF instance .
        """
        if not all(isinstance(ecdf, EmpiricalCDF) for ecdf in other_ecdfs):
            raise ValueError("All arguments must be instances of EmpiricalCDF.")

        combined_probabilities = defaultdict(float)

        all_values = set()
        for ecdf in [self] + other_ecdfs:
            for value, prob in ecdf.normalized_cdf:
                all_values.add(value)

        for ecdf in [self] + other_ecdfs:
            for value, prob in ecdf.normalized_cdf:
                combined_probabilities[value] += prob

        total_probability = sum(combined_probabilities.values())
        if total_probability == 0:
            raise ValueError("Combined probabilities cannot sum to zero.")

        combined_cdf = []
        cumulative_prob = 0
        for value in sorted(all_values):
            cumulative_prob += combined_probabilities[value]
            combined_cdf.append((value, cumulative_prob / total_probability))

        return EmpiricalCDF([s[0] for s in combined_cdf])

