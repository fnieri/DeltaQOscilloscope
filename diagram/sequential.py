from diagram.component import DiagramComponent
from utils.math_utils import *

class SequentialOperator():
    def __init__(self, name: str):
        self.name = name
        self.next: DiagramComponent = None

    def set_next_operator(self, next: DiagramComponent):
        self.next = next

    def calculate_dq(self, previous_pdf, previous_bins):
        # First operator in system
        if previous_pdf is None and previous_bins is None:
            pdf, values = self.next.calculate_dq()
        elif self.next is not None:
            pdf, bins = convolve_pdf(previous_pdf, previous_bins, *self.next.calculate_dq())
            return pdf, bins
        else:
            return previous_pdf, previous_bins
