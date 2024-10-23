"""
Author: Francesco Nieri
Class representing a probabilistic operator in an outcome diagram
"""

from diagram.ObservationPoint import ObservationPoint
from diagram.DiagramComponent import DiagramComponent
from utils.EmpiricalCDF import EmpiricalCDF


class ProbabilisticOperator(DiagramComponent):
    def __init__(self, name, following_components):
        super().__init__(name)
        self.following_components_and_probabilities: dict[ObservationPoint, int] = following_components
        if not self.check_probabilities_sum():
            raise ValueError("The sum of probabilities must equal 1.")

    def check_probabilities_sum(self) -> bool:
        """
        Check if the probabilities supplied actually sum to 1
        :return:
        """
        total_prob = sum(self.following_components_and_probabilities.values())
        return abs(total_prob - 1) < 1e-6

    def calculate_dq(self, *args) -> EmpiricalCDF:
        """
        Calculate the DeltaQ for the probabilistic operator
        :param args:
        :return:
        """
        cdfs = []
        for component, probability in self.following_components_and_probabilities.items():
            cdf = component.calculate_dq() * probability
            cdfs.append(cdf)
        cdf = cdfs[0].add_cdfs(cdfs[1:])
        return cdf

    def is_plottable(self):
        return False