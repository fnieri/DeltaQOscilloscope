"""
Author: Francesco Nieri

Class representing an observation point in an outcome diagram
"""

import numpy as np

from diagram.DiagramComponent import DiagramComponent
from diagram.SequentialOperator import SequentialOperator
from utils.EmpiricalCDF import EmpiricalCDF

class ObservationPoint(DiagramComponent):
    def __init__(self, name="default"):
        super().__init__(name)
        self.values = []
        self.next: SequentialOperator = None

    def set_next(self, next):
        self.next = next

    def add_value(self, value):
        self.values.append(value)

    def get_empirical_cdf(self) -> EmpiricalCDF:
        """
        Return empirical ecdf class based on values of observation point
        :return: EmpiricalCDF
        """
        values_copy = self.values.copy()

        lower_percentile, upper_percentile = 1, 99
        lower_bound = np.percentile(values_copy, lower_percentile)
        upper_bound = np.percentile(values_copy, upper_percentile)

        values_clipped = np.clip(values_copy, lower_bound, upper_bound)

        return EmpiricalCDF(values_clipped)

    def calculate_dq(self, *args) -> EmpiricalCDF:
        """
        Calculate the resulting delta Q for the ObservationPoint
        This method delegates the calculations to the next element who is typically a SequentialOperator

        :param args: Nothing should be supplied
        :return: Return the DeltaQ calculated from this ObservationPoint
        """
        return self.next.calculate_dq(self.get_empirical_cdf())

    def is_plottable(self):

        return True