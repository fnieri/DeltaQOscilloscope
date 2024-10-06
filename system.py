from componentsDict import ComponentsDict
from diagram.ObservationPoint import ObservationPoint

class System:
    def __init__(self):
        self.components = ComponentsDict()
        self.first_component = None
        self.last_component = None

    def add_component(self, component):
        self.components[component.name] = component

    def add_time(self, name, time):
        self.components[name].add_value(time)

    def set_first_component(self, name):
        self.first_component = self.components[name]

    def set_last_component(self, name):
        self.last_component = self.components[name]

    def calculate_dq(self):
        # Use None, None as we will calculate from the first sequential operator and we can't convolute from previous (inexistent)
        self.first_component.calculate_dq(None, None)
