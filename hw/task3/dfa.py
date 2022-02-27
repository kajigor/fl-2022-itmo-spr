import json
from typing import Dict, List
from collections import defaultdict
from dfa_parser import DFAParser
from pyparsing import ParseException


class DFA:
    def __init__(self):
        self._parser = DFAParser()

        self.states: List[str] = []
        self.alphabet: List[str] = []
        self.init_state: str = ""
        self.final_states: List[str] = []
        self.transitions: Dict[Dict[List[str]]] = defaultdict(lambda: defaultdict(list))

    def _read(self, input_path: str):
        with open(input_path, "r") as f:
            parse_results = self._parser.parse(f.read())
        self.states = parse_results["states"]
        self.symbols = parse_results["symbols"]
        self.init_state = parse_results["init_state"][0]
        self.final_states = parse_results["final_states"]

        for trans in parse_results["transitions"]:
            self.transitions[trans["from"]][trans["to"]].append(trans["symbol"])

    def _write(self, output_path: str):
        result = {
            "states": self.states,
            "symbols": self.symbols,
            "init_state": self.init_state,
            "final_states": self.final_states,
            "transitions": self.transitions,
        }

        with open(output_path, "w") as f:
            json.dump(result, f, indent=4)

    def parse(self, input_path: str):
        """
        This method parses DFA from `input_path` and writes results to `input_path.out`.
        """
        try:
            self._read(input_path)
            self._write(f"{input_path}.out")
        except ParseException:
            with open(f"{input_path}.out", "w") as f:
                f.write("Syntax error\n")
