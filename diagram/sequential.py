from diagram.component import DiagramComponent
from utils.math_utils import *

class SequentialOperator():
    def __init__(self, name: str):
        self.name = name
        self.next: DiagramComponent = None

    def set_next_operator(self, next: DiagramComponent):
        self.next = next

    def calculate_dq(self, previous_pdf, previous_bins):
        if self.next is not None:
            return convolve(previous_pdf, previous_bins, *self.next.calculate_dq())
        return None