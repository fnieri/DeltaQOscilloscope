import pydot

from diagram.ObservationPoint import ObservationPoint
from diagram.ProbabilisticOperator import ProbabilisticOperator
from diagram.SequentialOperator import SequentialOperator

def create_system_graph(system):
    graph = pydot.Dot(graph_type="digraph", rankdir="LR")

    nodes = {}

    for component in system.components.values():
        if isinstance(component, SequentialOperator):
            node = pydot.Node(component.name, shape="square", width="0.2", height="0.2", label="")
        elif isinstance(component, ProbabilisticOperator):
            node = pydot.Node(component.name, label="⇋")
        else:
            node = pydot.Node(component.name)

        graph.add_node(node)
        nodes[component] = node

    first_component = system.first_component
    first_node = nodes[first_component]
    first_node.set("rank", "min")

    for component in system.components.values():
        if isinstance(component, SequentialOperator):
            if component.next:
                next_node = nodes[component.next]
                edge = pydot.Edge(nodes[component], next_node)
                graph.add_edge(edge)

        if isinstance(component, ObservationPoint) and component.next:
            next_node = nodes[component.next]
            edge = pydot.Edge(nodes[component], next_node)
            graph.add_edge(edge)

        if isinstance(component, ProbabilisticOperator):
            for target, prob in component.following_components_and_probabilities.items():
                edge = pydot.Edge(nodes[component], nodes[target], label=str(prob))
                graph.add_edge(edge)

    return graph

