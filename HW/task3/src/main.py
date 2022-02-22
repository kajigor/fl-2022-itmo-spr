import json
import sys
import networkx as nx
from edge_labels import my_draw_networkx_edge_labels
import matplotlib.pyplot as plt
from pyvis_show import plot_g_pyviz


class DFA:
    def __init__(self, data, input_file):
        self.data = data
        self.input_file = input_file

    def get_initial_state(self):
        return self.data["initial_state"]

    def get_terminal_states(self):
        return self.data["terminal_states"]

    def get_glossary(self):
        return self.data["glossary"]

    def get_edges(self):
        return self.data["edges"]

    def get_states(self):
        return self.data["states"]

    def print_to_file(self):
        try:
            with open(self.input_file + ".out", '+w') as output_file:
                json.dump(self.data, output_file, ensure_ascii=False, indent=2)
        except FileNotFoundError:
            print("Can't open output file!")
            return

    def show_graph_1(self):
        graph = nx.MultiDiGraph()

        node_list = self.data["states"]
        edge_list = self.data["edges"]
        initial_state = self.data["initial_state"]
        terminal_states = self.data["terminal_states"]

        # Создаем словарь из ребра и его названия
        edge_names = {}
        for state, values in edge_list.items():
            for other_state, edge_name in values:
                if (state, other_state) in edge_names:
                    edge_names[(state, other_state)] += "; " + edge_name
                else:
                    edge_names[(state, other_state)] = edge_name

        # Добавляем ребра и вершины
        graph.add_nodes_from(node_list)
        graph.add_edges_from(edge_names.keys())

        # Задаем layout для вершин
        pos = nx.planar_layout(graph)

        # Ищем начальное и терминальные состояния, чтобы пометить их цветом.
        # Цвета выбираются так:
        # Желтый - начальное состояние
        # Красный - терминальное состояние
        # Розовый - остальные состояния
        initial_state_pos = node_list.index(initial_state)
        terminal_states_pos = [ind for ind, state in enumerate(node_list) if state in terminal_states]
        node_color = [1 if ind == initial_state_pos else
                      0.7 if ind in terminal_states_pos else
                      0.4 for ind in range(len(node_list))]

        # Изгиб ребер
        rad = 0.2

        nx.draw_networkx(graph, pos=pos, nodelist=node_list, node_color=node_color,
                         cmap=plt.get_cmap('spring'), connectionstyle=f'arc3, rad = {rad}')

        # Модифицированная функция добавления названий к искривленным дугам.
        my_draw_networkx_edge_labels(graph, pos=pos, edge_labels=edge_names, rad=rad)

        plt.show()

    def show_graph_2(self):
        graph = nx.MultiDiGraph()

        node_list = self.data["states"]
        edge_list = self.data["edges"]
        initial_state = self.data["initial_state"]
        terminal_states = self.data["terminal_states"]

        # Создаем словарь из ребра и его названия
        edge_names = {}
        for state, values in edge_list.items():
            for other_state, edge_name in values:
                if (state, other_state) in edge_names:
                    edge_names[(state, other_state)] += "; " + edge_name
                else:
                    edge_names[(state, other_state)] = edge_name

        # Добавляем ребра и вершины.
        # Цвета состояний:
        # Зеленый - для начального состояния
        # Розовый - для терминальных
        # Синий - для остальных
        for node in node_list:
            graph.add_node(node, color=('green' if node == initial_state
                                        else 'pink' if node in terminal_states
                                        else 'blue'))

        for states, label in edge_names.items():
            graph.add_edge(*states, label=label)

        plot_g_pyviz(graph)


def main():
    if len(sys.argv) == 1:
        print("No file was provided")
        return

    file_name = sys.argv[1]

    try:
        with open(file_name, 'r') as file:
            data = json.load(file)
    except FileNotFoundError:
        print("Can't open the file")
        return

    dfa = DFA(data, file_name)

    dfa.show_graph_1()
    dfa.print_to_file()


if __name__ == "__main__":
    main()
