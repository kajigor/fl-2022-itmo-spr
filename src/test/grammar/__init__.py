import sys
from antlr4 import *
from antlr4.tree.Trees import Trees
from src.grammar.gen.MyGrammarLexer import MyGrammarLexer
from src.grammar.gen.MyGrammarParser import MyGrammarParser
from src.grammar.gen.MyGrammarListener import MyGrammarListener


def main(argv):
    input = FileStream(argv[1])
    lexer = MyGrammarLexer(input)
    stream = CommonTokenStream(lexer)
    parser = MyGrammarParser(stream)
    tree = parser.startRule()

    print(Trees.toStringTree(tree, None, parser))

    printer = MyGrammarListener()
    walker = ParseTreeWalker()
    walker.walk(printer, tree)


if __name__ == '__main__':
    main(sys.argv)
