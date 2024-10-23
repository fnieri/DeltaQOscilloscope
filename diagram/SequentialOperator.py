from diagram.DiagramComponent import DiagramComponent
from utils.EmpiricalCDF import EmpiricalCDF


class SequentialOperator(DiagramComponent):
    def __init__(self, name: str):
        super().__init__(name)
        self.next: DiagramComponent = None

    def set_next_operator(self, next: DiagramComponent):
        self.next = next

    def calculate_dq(self, previous_ecdf: EmpiricalCDF):
        if previous_ecdf is None: #Operator is first in system
            return self.next.calculate_dq()

        elif self.next is not None:
            return previous_ecdf @ self.next.calculate_dq()

        else:
            return previous_ecdf

    def is_plottable(self):
        return False