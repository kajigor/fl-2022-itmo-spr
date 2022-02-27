import os
from dfa import DFA


if __name__ == "__main__":
    dfa = DFA()
    for example_type in ["correct", "wrong"]:
        for fname in os.listdir(os.path.join("examples", example_type)):
            dfa.parse(os.path.join("examples", example_type, fname))
