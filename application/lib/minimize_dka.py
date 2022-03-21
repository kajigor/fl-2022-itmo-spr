from typing import Set
from functools import reduce

from .automat import Automatos
from .transform_to_dka import transformation


def get_reachable_states(dka: Automatos):
    to_process = [(dka.root, dka.root.name)]
    processed = {dka.root.name}
    while to_process:
        current, name = to_process.pop()
        for symbol in dka.alfabet:
            if symbol not in current.children or current.children[symbol][0].name in processed:
                continue
            next_state = current.children[symbol]
            to_process.append((next_state[0], next_state[0].name))
            processed.add(next_state[0].name)
    return processed


class DisjointSet:
    def __init__(self, items):
        self._disjoint_set = list()
        for item in set(items):
            self._disjoint_set.append([item])

    def _get_index(self, elem):
        for set in self._disjoint_set:
            for item in set:
                if item == elem:
                    return self._disjoint_set.index(set)
        return None

    def find(self, elem):
        for s in self._disjoint_set:
            if elem in s:
                return s
        return None

    def find_set(self, elem):
        s = self._get_index(elem)

        return reduce(lambda x, y: x+y, self._disjoint_set[s]) if s is not None else None

    def union(self, elem1, elem2):
        set1 = self._get_index(elem1)
        set2 = self._get_index(elem2)

        if set1 != set2:
            self._disjoint_set[set1] += self._disjoint_set[set2]
            del self._disjoint_set[set2]

    def get(self):
        return self._disjoint_set


def minimize(dka: Automatos) -> Automatos:
    dka = transformation(dka)
    if len(dka.states.keys()) == 0:
        return dka

    reachables: Set = get_reachable_states(dka)
    states = sorted(dka.name_of_states.intersection(reachables))

    final_states = {name for name in dka.name_of_states if dka.states[name].is_terminal}
    table = {}
    for i, state1 in enumerate(states):
        for state2 in states[i + 1:]:
            table[(state1, state2)] = (state1 in final_states) != (state2 in final_states)

    flag = True
    while flag:
        flag = False
        for i, state1 in enumerate(states):
            for state2 in states[i + 1:]:

                if table[(state1, state2)]:
                    continue

                for symbol in dka.alfabet:
                    t1 = dka.states[state1].children.get(symbol, None)
                    t2 = dka.states[state2].children.get(symbol, None)

                    if t1 is not None and t2 is not None and t1[0].name != t2[0].name:
                        marked = table[(t1[0].name, t2[0].name) if t1[0].name < t2[0].name else (t2[0].name, t1[0].name)]
                        flag = flag or marked
                        table[(state1, state2)] = marked

                        if marked:
                            break

    d = DisjointSet(dka.name_of_states)
    for k, v in table.items():
        if not v:
            d.union(k[0], k[1])

    result = Automatos(dka.alfabet)
    for eq_state in d.get():
        result.addState(reduce(lambda x, y: x+y, eq_state))

    result.makeRoot(str(d.find_set(dka.root.name)))

    for s in d.get():
        for state1 in s:
            if state1 in final_states:
                result.makeTerminal(d.find_set(state1))
                break
    for k, v in dka.transitions.items():
        result.addAction(d.find_set(k[0]), d.find_set(v), k[1])

    return result
