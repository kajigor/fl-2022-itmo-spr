import argparse
from dfa import DFA


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Parse given file with DFA description.")
    parser.add_argument("input_path", metavar="<input_path>", type=str, help="path to input file")
    args = parser.parse_args()

    dfa = DFA()
    dfa.parse(args.input_path)
