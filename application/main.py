#!/usr/bin/env python
from collections import defaultdict

from lib.transform_to_dka import transformation

import lib.parser as parser
import sys
import networkx as nx
import matplotlib.pyplot as plt


def main(args):
    if len(args) < 1:
        print("No file specified!")
        return

    try:
        automat = parser.parseFromFile(args[1])
    except parser.ParseError as e:
        print(e.message)
        return

    while True:
        arguments = sys.stdin.readline().split(maxsplit=1)
        if len(arguments) == 1:
            command = arguments[0]
            if command == "exit":
                return
            elif command == "visualize":
                visualize(automat)
            elif command in ["check"]:
                print("Not enough arguments")
            else:
                print(f"Command {command} not found")

        elif len(arguments) > 1:
            command, arguments = arguments
            if command == "check":
                automat_for_check = transformation(automat)
                if automat_for_check.go(arguments.strip()):
                    print("String is accepted")
                else:
                    print("String is not accepted")
            else:
                print(f"Command {command} not found")

        else:
            print("Not enough arguments")


def visualize(automat):
    g = nx.DiGraph()

    node_list = list(automat.states.keys())
    initial_state = automat.root.name
    node_color = ['grey' if ind == node_list.index(initial_state) else
                  'white' for ind in range(len(node_list))]
    terminal_states = [s.name for s in automat.states.values() if s.is_terminal]
    other_states = [s.name for s in automat.states.values() if not s.is_terminal]

    for name, state in automat.states.items():
        g.add_node(name)

    temp = defaultdict(list)
    for name, state in automat.states.items():
        for lit, children in state.children.items():
            for c in children:
                temp[(name, c.name)].append(lit)

    edge_labels = {}
    for k, v in temp.items():
        g.add_edge(k[0], k[1])
        edge_labels[(k[0], k[1])] = ','.join(v)

    nx.draw_networkx_edge_labels(g, pos=nx.planar_layout(g), edge_labels=edge_labels)
    nx.draw_networkx_nodes(g, pos=nx.planar_layout(g), nodelist=terminal_states, node_shape='o', node_size=900)
    nx.draw_networkx_nodes(g, pos=nx.planar_layout(g), nodelist=terminal_states, node_shape='o', node_size=700, node_color='white')
    nx.draw_networkx_nodes(g, pos=nx.planar_layout(g), nodelist=terminal_states, node_shape='o', node_size=400)
    nx.draw_networkx_nodes(g, pos=nx.planar_layout(g), nodelist=other_states)
    nx.draw_networkx(g, pos=nx.planar_layout(g), node_color=node_color)
    plt.show()


if __name__ == "__main__":
    main(sys.argv)
