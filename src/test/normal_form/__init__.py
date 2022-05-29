import os

from src.grammar.gen.MyGrammarLexer import MyGrammarLexer
from src.grammar.gen.MyGrammarParser import MyGrammarParser
from src.mapper.TreeMapper import make_dict
from pprint import pprint
from antlr4 import FileStream, CommonTokenStream
from src.normal_form.transforms import *

from src.report.tools import Report

import unittest


class TestTransformer(unittest.TestCase):

    def test_unreachable(self):
        inp = FileStream('unreach.test')
        lexer = MyGrammarLexer(inp)
        stream = CommonTokenStream(lexer)
        parser = MyGrammarParser(stream)
        tree = parser.startRule()

        start_item, res_dict = make_dict(tree)
        res_dict = delete_unreachable(res_dict, start_item)
        trans_res = sorted(res_dict.items()).__str__()
        exp_res = '[(\'A\', \"a\")]'
        self.assertEqual(trans_res, exp_res)

    def test_nongenerating(self):
        inp = FileStream('nongen.test')
        lexer = MyGrammarLexer(inp)
        stream = CommonTokenStream(lexer)
        parser = MyGrammarParser(stream)
        tree = parser.startRule()

        _, res_dict = make_dict(tree)
        res_dict = delete_nongenerating(res_dict)
        trans_res = sorted(res_dict.items()).__str__()
        exp_res = '[(\'A\', \"a\")]'
        self.assertEqual(trans_res, exp_res)

    def test_long_right_parts(self):
        inp = FileStream('long_part.test')
        lexer = MyGrammarLexer(inp)
        stream = CommonTokenStream(lexer)
        parser = MyGrammarParser(stream)
        tree = parser.startRule()

        _, res_dict = make_dict(tree)
        res_dict = delete_long_right_part(res_dict)
        trans_res = sorted(res_dict.items()).__str__()
        exp_res = '[(\'A\', \"a\" _0), (\'_0\', \"a\" \"a\")]'
        self.assertEqual(trans_res, exp_res)

    def test_delete_epsilon(self):
        inp = FileStream('epsilon.test')
        lexer = MyGrammarLexer(inp)
        stream = CommonTokenStream(lexer)
        parser = MyGrammarParser(stream)
        tree = parser.startRule()

        start_item, res_dict = make_dict(tree)
        res_dict = delete_epsilons(res_dict, start_item)
        trans_res = sorted(res_dict.items()).__str__()
        exp_res = '[(\'B\', \"b\"), (\'BC\', B | C), (\'C\', \"c\"), (\'_START\', #e | BC)]'
        self.assertEqual(trans_res, exp_res)

    def test_chain_products(self):
        inp = FileStream('chain.test')
        lexer = MyGrammarLexer(inp)
        stream = CommonTokenStream(lexer)
        parser = MyGrammarParser(stream)
        tree = parser.startRule()

        start_item, res_dict = make_dict(tree)
        res_dict = delete_chain_products(res_dict)
        res_dict = delete_useless_non_terminal(res_dict, 'A')
        trans_res = sorted(res_dict.items()).__str__()
        exp_res = '[(\'A\', \"d\")]'
        self.assertEqual(trans_res, exp_res)

    def test_right_terminals(self):
        inp = FileStream('right_term.test')
        lexer = MyGrammarLexer(inp)
        stream = CommonTokenStream(lexer)
        parser = MyGrammarParser(stream)
        tree = parser.startRule()

        start_item, res_dict = make_dict(tree)
        res_dict = delete_right_terminals(res_dict)
        trans_res = sorted(res_dict.items()).__str__()
        exp_res = '[(\'A\', __a __c), (\'__a\', "a"), (\'__c\', \"c\")]'
        self.assertEqual(trans_res, exp_res)

    def test_empty(self):
        inp = FileStream('empty_res_grammar.test')
        report = Report()
        lexer = MyGrammarLexer(inp)
        stream = CommonTokenStream(lexer)
        parser = MyGrammarParser(stream)
        tree = parser.startRule()

        start_item, res_dict = make_dict(tree)
        res_dict, _ = transform(res_dict, start_item, report)
        self.assertEqual(res_dict, dict())
        os.remove('cnf_steps_STRING.txt')




if __name__ == '__main__':
    unittest.main()
