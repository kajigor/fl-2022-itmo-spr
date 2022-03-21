from queue import Queue

from automat import Automatos, State
from parser import parseFromFile


def transformation(nka_automa: Automatos) -> Automatos:
    dka_automa = Automatos(nka_automa.alfabet)
    dka_automa.addState(nka_automa.root.name)
    dka_automa.makeRoot(nka_automa.root.name)
    que = Queue()
    que.put(nka_automa.root)
    while not que.empty():
        cur_state = que.get()
        for symb, states in cur_state.children.items():
            new_state_name = "_".join(sorted([state.name for state in states]))
            new_childrens = make_new_children(nka_automa.alfabet, states)
            terminals = list(filter(lambda state: state.is_terminal, states))

            new_state = State(new_state_name)
            new_state.children = new_childrens
            new_state.is_terminal = (len(terminals) > 0)

            if new_state_name not in dka_automa.states:
                que.put(new_state)

                dka_automa.addState(new_state_name)
                if new_state.is_terminal:
                    dka_automa.makeTerminal(new_state_name)
            dka_automa.addAction(cur_state.name, new_state_name, symb)

    return dka_automa



def make_new_children(alfabet, states_array):
    children = {}
    for symb in alfabet:
        symb_states = set()
        for state in states_array:
            if symb in state.children:
                symb_states = symb_states.union(state.children[symb])
        children[symb] = list(symb_states)
    return children

