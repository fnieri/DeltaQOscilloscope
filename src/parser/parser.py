
import json
from lark import Lark, Transformer

grammar = """
%import common.WS
%ignore WS

start: system (";" probe_list)?  // System first, optional probes

system: component+

component: IDENTIFIER ("->" component)?  
        | BEHAVIOR_TYPE ":" IDENTIFIER "(" component_list ")" ("->" component)?  

component_list: component ("," component)*

probe_list: "[" IDENTIFIER ("," IDENTIFIER)* "]"  // List of probes

BEHAVIOR_TYPE: "f" | "a" | "p"

IDENTIFIER: /[a-zA-Z0-9_]+/
%ignore WS
"""

class ComponentTransformer(Transformer):
    def start(self, items):
        system_data = items[0]  # First part is the system
        probes = items[1] if len(items) > 1 else []  # Probes if present
        return {"system": system_data, "probes": probes}

    def system(self, items):
        return items  # Return system components

    def component(self, items):
        if len(items) == 1:  
            return {"name": items[0]}  # Simple component
        
        if len(items) >= 3 and isinstance(items[0], str):  # Behavior-based component
            behavior = items[0]
            name = items[1]
            component_data = {"name": name, "type": behavior}
            children = items[2] if isinstance(items[2], list) else []
            if children:
                component_data["children"] = children
            if len(items) > 3 and isinstance(items[3], dict):
                component_data["next"] = items[3]
            return component_data
        
        # Regular component with "->" linkage
        component_data = {"name": str(items[0])}
        if isinstance(items[1], dict):
            component_data["next"] = items[1]

        return component_data

    def component_list(self, items):
        return items  

    def probe_list(self, items):
        return items  # Return as list of probe names

    def BEHAVIOR_TYPE(self, token):
        return str(token)

    def IDENTIFIER(self, token):
        return str(token)  # Convert token to string

# Create the parser
parser = Lark(grammar, start="start", parser="earley")
transformer = ComponentTransformer()

def parse_and_save_json(input_string, filename="parsed_system.json"):
    tree = parser.parse(input_string)
    transformed_tree = transformer.transform(tree)
    with open(filename, "w") as json_file:
        json.dump(transformed_tree, json_file, indent=2)
    return filename  # Return filename for reference

# Example Usage:
# parse_and_save_json("worker_1 -> f:ftf_1(worker_2 -> start -> end, o4 -> p:probab(o5 -> o6 -> p:carm(2 -> 3))) -> o7 ; [probe1, probe2]")
rint(data)
