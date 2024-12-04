class DiagramComponent:
    def __init__(self, name: str):
        self.name = name

    def calculate_dq(self, *args):
        pass

    def is_plottable(self):
        return False
