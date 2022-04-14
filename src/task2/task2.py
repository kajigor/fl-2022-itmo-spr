from antlr4 import *
from antlr_files.AutomataLexer import AutomataLexer
from antlr_files.AutomataParser import AutomataParser
from antlr_files.AutomataVisitor import AutomataVisitor

from antlr4.tree.Trees import Trees

from contextlib import redirect_stdout, redirect_stderr


class MyAutomataVisitor(AutomataVisitor):
    shift = 0

    def print_node(self, ctx, node_name):
        self.shift += 1
        child = self.visitChildren(ctx)
        print(" " * self.shift + node_name + ": " + ctx.getText())
        self.shift -= 1
        return child

    def visitS(self, ctx: AutomataParser.SContext):
        return self.print_node(ctx, "S")

    # Visit a parse tree produced by AutomataParser#lp.
    def visitLp(self, ctx: AutomataParser.LpContext):
        return self.print_node(ctx, "LP")

    # Visit a parse tree produced by AutomataParser#a.
    def visitA(self, ctx: AutomataParser.AContext):
        return self.print_node(ctx, "A")

    # Visit a parse tree produced by AutomataParser#lao.
    def visitLao(self, ctx: AutomataParser.LaoContext):
        return self.print_node(ctx, "LAO")

    # Visit a parse tree produced by AutomataParser#lc.
    def visitLc(self, ctx: AutomataParser.LcContext):
        return self.print_node(ctx, "LC")

    # Visit a parse tree produced by AutomataParser#ee.
    def visitEe(self, ctx: AutomataParser.EeContext):
        return self.print_node(ctx, "EE")

    # Visit a parse tree produced by AutomataParser#eo.
    def visitEo(self, ctx: AutomataParser.EoContext):
        return self.print_node(ctx, "EO")

    # Visit a parse tree produced by AutomataParser#ll.
    def visitLl(self, ctx: AutomataParser.LlContext):
        return self.print_node(ctx, "LL")


if __name__ == "__main__":
    # Пробуем разобрать правильный автомат
    with open("result/log_correct.txt", "w") as log_correct_file:
        with redirect_stdout(log_correct_file), redirect_stderr(log_correct_file):
            with open("correct_example.json", 'r') as correct_file:
                data = correct_file.read()

            data_stream = InputStream(data)
            # lexer
            lexer = AutomataLexer(data_stream)
            stream = CommonTokenStream(lexer)
            # parser
            parser = AutomataParser(stream)
            tree = parser.s()

            # evaluator
            print("\nCustom print of the tree:\n")
            visitor = MyAutomataVisitor()
            visitor.visit(tree)

            print("---" * 30)
            print("Built-in print:")
            print(Trees.toStringTree(tree, None, parser))

    # Пробуем разобрать неправильный автомат
    with open("result/log_incorrect.txt", "w") as log_incorrect_file:
        with redirect_stdout(log_incorrect_file), redirect_stderr(log_incorrect_file):
            with open("incorrect_example.json", 'r') as correct_file:
                data = correct_file.read()

            data_stream = InputStream(data)
            # lexer
            lexer = AutomataLexer(data_stream)
            stream = CommonTokenStream(lexer)
            # parser
            parser = AutomataParser(stream)
            tree = parser.s()

            # evaluator
            print("\nCustom print of the tree:\n")
            visitor = MyAutomataVisitor()
            visitor.visit(tree)

            print("---" * 30)
            print("Built-in print:")
            print(Trees.toStringTree(tree, None, parser))
