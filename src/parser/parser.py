
import json
from lark import Lark, Transformer

grammar = """
%import common.WS
%ignore WS

start: definition* system?  //Subsystem first, system second
definition: IDENTIFIER "=" component+ ";"

system: "system" "=" component+

component: outcome ("->" component)?  
        | BEHAVIOR_TYPE ":" IDENTIFIER "(" component_list ")" ("->" component)?  
        | probe ("->" component)?

outcome: IDENTIFIER
probe: PROBE_IDENTIFIER ":" IDENTIFIER 
component_list: component ("," component)+

PROBE_IDENTIFIER: "s"
BEHAVIOR_TYPE: "f" | "a" | "p"

IDENTIFIER: /[a-zA-Z0-9_]+/
%ignore WS
"""

class ComponentTransformer(Transformer):
    def start(self, items):
        definitions = items[:-1] if len(items) > 1 else items
        system = items[-1] if len(items) > 1 else None
        return {"probes": definitions, "system": system}

    def definition(self, items):
        components = items[1:]
        if len(components) == 1:
            components = components[0]
        return {"name": items[0], "components": components}

    def system(self, items):
        components = items
        if len(components) == 1:
            components = components[0]
        return {"name": "system", "components": components}

    def component(self, items):
        if len(items) == 1 and isinstance(items[0], dict):
            return items[0]  

        if len(items) == 2 and isinstance(items[0], dict):
            component_data = items[0]
            component_data["next"] = items[1]
            return component_data

        if len(items) >= 3 and isinstance(items[0], str) and items[0] in ["f", "a", "p"]:
            behavior, name, children = items[0], items[1], self.flatten_list(items[2:])
            return {
                "name": name,
                "type": behavior,
                "children": children
            }

        return items

    def outcome(self, items):
        return {"name": items[0], "type": "o"}  

    def probe(self, items):
        return {"name": items[1], "type": "s"}  

    def component_list(self, items):
        return self.flatten_list(items)

    def flatten_list(self, items):
        """Flattens nested lists while ensuring structure remains intact."""
        flattened = []
        for item in items:
            if isinstance(item, list):
                flattened.extend(self.flatten_list(item))
            else:
                flattened.append(item)
        return flattened

    def BEHAVIOR_TYPE(self, token):
        return str(token)

    def IDENTIFIER(self, token):
        return str(token)

    def PROBE_IDENTIFIER(self, token):
        return str(token)

# Create the parser
parser = Lark(grammar, start="start", parser="earley")
transformer = ComponentTransformer()

def parse_and_save_json(input_string, filename="parsed_system.json"):
    tree = parser.parse(input_string)
    transformed_tree = transformer.transform(tree)
    with open(filename, "w") as json_file:
        json.dump(transformed_tree, json_file, indent=2)
    return filename

# Example Usage:
parse_and_save_json("probe1 = o1 -> o2 -> s:o3; probe2 = o4 -> f:ftf1(o5 -> p:probab(o6, o7), o8); system = s:probe1 -> s:probe2 -> p:probab(o7, o9);")

