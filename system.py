from componentsDict import ComponentsDict
from diagram.ObservationPoint import ObservationPoint
from diagram.component import DiagramComponent


class System:
    def __init__(self):
        self.components = ComponentsDict()
        self.first_component = None
        self.last_component = None

    def add_component(self, component):
        self.components[component.name] = component

    def add_time(self, name: str, time: int):
        self.components[name].add_value(time)

    def set_first_component(self, name: str):
        self.first_component = self.components[name]

    def set_last_component(self, name: str):
        self.last_component = self.components[name]

    def calculate_dq(self):
        # Use None, None as we will calculate from the first sequential operator and we can't convolute from previous (inexistent)
        pdf, values = self.first_component.calculate_dq(None, None)
        return pdf, values

    def get_all_plottable_components(self) -> list[DiagramComponent]:
        plottable_components = []
        for component in self.components.values():
            if component.is_plottable():
                plottable_components.append(component)
        return plottable_components