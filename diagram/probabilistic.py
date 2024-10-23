from diagram.ObservationPoint import ObservationPoint
from diagram.component import DiagramComponent
from utils.math_utils import multiply_cdf, add_cdfs, compute_cdf_from_pdf, compute_pdf_from_cdf
import matplotlib.pyplot as plt

class ProbabilisticOperator(DiagramComponent):
    def __init__(self, name, following_components):
        super().__init__(name)
        self.following_components_and_probabilities: dict[ObservationPoint, int] = following_components
        if not self.check_probabilities_sum():
            raise ValueError("The sum of probabilities must equal 1.")

    def check_probabilities_sum(self):
        total_prob = sum(self.following_components_and_probabilities.values())
        return abs(total_prob - 1) < 1e-6

    def calculate_dq(self, *args):
        cdfs = []
        for component, probability in self.following_components_and_probabilities.items():
            pdf, values = component.calculate_dq()
            cdf = compute_cdf_from_pdf(pdf, values)
            multiplied_cdf = multiply_cdf(cdf[0], probability)
            cdfs.append((multiplied_cdf, values))
            print(len(cdf[1]    ))
        print(cdfs)
        values, cdf = add_cdfs(cdfs)
        pdf, values = compute_pdf_from_cdf(cdf, values)
        cdf = compute_cdf_from_pdf(pdf, values)
        print()
        print(cdf)
        return pdf, values

    def is_plottable(self):
        return False