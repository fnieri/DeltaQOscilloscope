import numpy as np

from diagram.component import DiagramComponent
from diagram.sequential import SequentialOperator
from utils.math_utils import convolve_pdf
import matplotlib.pyplot as plt

class ObservationPoint(DiagramComponent):
    def __init__(self, name="default"):
        super().__init__(name)
        self.values = []
        self.next: SequentialOperator = None

    def set_next(self, next):
        self.next = next

    def get_min_value(self):
        return np.min(self.values)

    def get_max_value(self):
        return np.max(self. values)

    def get_25_percentile(self):
        return np.percentile(self.values, 25)

    def get_50_percentile(self):
        return np.percentile(self.values, 50)

    def get_75_percentile(self):
        return np.percentile(self.values, 75)

    def add_value(self, value):
        self.values.append(value)

    def get_cdf_and_sorted_values(self):

        """
        https://stats.stackexchange.com/questions/381588/how-does-this-code-find-the-cdf

        Return CDF and sorted values from value points of observation point
        :return: Cdf and sorted data
        """
        values_copy = self.values.copy()

        lower_percentile, upper_percentile = 1, 99
        lower_bound = np.percentile(values_copy, lower_percentile)
        upper_bound = np.percentile(values_copy, upper_percentile)

        values_clipped = np.clip(values_copy, lower_bound, upper_bound)

        sorted_data = np.sort(values_clipped)

        return np.arange(1, len(sorted_data) + 1) / len(sorted_data), sorted_data

    def get_pdf_and_bin_edges(self, bins=100):
        values_copy = self.values.copy()
        lower_percentile, upper_percentile = 1, 99
        lower_bound = np.percentile(values_copy, lower_percentile)
        upper_bound = np.percentile(values_copy, upper_percentile)

        values_clipped = np.clip(values_copy, lower_bound, upper_bound)


        pdf, bin_edges = np.histogram(values_clipped, bins=bins, density=True)
        bin_centers = (bin_edges[:-1] + bin_edges[1:]) / 2  # Compute bin centers
        return pdf, bin_centers

    def calculate_dq(self, *args):
        return self.next.calculate_dq(*self.get_pdf_and_bin_edges())

    def is_plottable(self):
        return True