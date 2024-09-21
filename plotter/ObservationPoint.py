import numpy as np


class ObservationPoint:
    def __init__(self):
        self.values = []

    def get_min_value(self):
        return np.min(self.values)

    def get_max_value(self):
        return np.max(self.values)

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
        Return CDF and sorted values from value points of observation point
        :return:
        """
        values_copy = self.values.copy()
        sorted_data = np.sort(values_copy)
        return np.arange(1, len(sorted_data) + 1) / len(sorted_data), sorted_data

    def get_pdf_and_bin_edges(self, bins=100):
        values_copy = self.values.copy()
        counts, bin_edges = np.histogram(values_copy, bins=bins, density=True)
        pdf = counts / np.sum(counts)
        return pdf, bin_edges[:-1]