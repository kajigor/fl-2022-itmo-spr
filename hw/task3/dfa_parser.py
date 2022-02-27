import pyparsing as pp
from typing import List, Dict, Union


class DFAParser:
    """
    Class for parsing my DFA grammar via pyparsing.
    """

    def __init__(self):
        pp.ParserElement.setDefaultWhitespaceChars(" ")
        var_name = pp.Word(pp.alphanums + "_")
        states = var_name[1, ...] + pp.Suppress(pp.LineEnd())
        symbols = var_name[1, ...] + pp.Suppress(pp.LineEnd())
        init_state = var_name + pp.Suppress(pp.LineEnd())
        final_states = var_name[1, ...] + pp.Suppress(pp.LineEnd())
        transition = pp.Dict(
            var_name.setResultsName("from")
            + pp.Suppress("->")
            + var_name.setResultsName("to")
            + pp.Suppress("(")
            + var_name.setResultsName("symbol")
            + pp.Suppress(")")
            + pp.Suppress(pp.LineEnd())
        )

        self.grammar = pp.Dict(
            states.setResultsName("states")
            + symbols.setResultsName("symbols")
            + init_state.setResultsName("init_state")
            + final_states.setResultsName("final_states")
            + pp.Group(transition)[1, ...].setResultsName("transitions")
        )

    def parse(self, s: str) -> Dict[str, Union[List[str], Dict[str, str]]]:
        return self.grammar.parseString(s).asDict()
