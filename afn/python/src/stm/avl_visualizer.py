"""
A simple module to draw graphs of AVL trees to png files.

Just call visualize(some_node, "some_file.png") to draw some_node (which should
be an avl.Node, or avl.empty)'s hierarchical representation to some_file.png.
Quite useful for debugging tree-related issues.
"""

import pydot
from stm_system import avl
from afn.utils.concurrent import AtomicInteger

next_id = AtomicInteger(1).get_and_add

def visualize(node, png_path):
    graph = pydot.Dot(graph_type="graph")
    draw_node(graph, node)
    graph.write_png(png_path)

def draw_node(graph, node):
    node_id = str(next_id())
    if node is avl.empty:
        graph.add_node(pydot.Node(node_id, label="<empty>", fontsize="9"))
    else:
        graph.add_node(pydot.Node(node_id, label='"' + repr(node.value) + 
                                  "\\nbalance: %s\\nheight: %s\\nweight: %s\"" %
                                  (node.balance, node.height, node.weight)))
        graph.add_edge(pydot.Edge(node_id, draw_node(graph, node.left)))
        graph.add_edge(pydot.Edge(node_id, draw_node(graph, node.right)))
    return node_id
        
