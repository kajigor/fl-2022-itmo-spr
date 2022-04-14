# Generated from Automata.g4 by ANTLR 4.10
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .AutomataParser import AutomataParser
else:
    from AutomataParser import AutomataParser

# This class defines a complete generic visitor for a parse tree produced by AutomataParser.

class AutomataVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by AutomataParser#s.
    def visitS(self, ctx:AutomataParser.SContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AutomataParser#lp.
    def visitLp(self, ctx:AutomataParser.LpContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AutomataParser#a.
    def visitA(self, ctx:AutomataParser.AContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AutomataParser#lao.
    def visitLao(self, ctx:AutomataParser.LaoContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AutomataParser#lc.
    def visitLc(self, ctx:AutomataParser.LcContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AutomataParser#ee.
    def visitEe(self, ctx:AutomataParser.EeContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AutomataParser#eo.
    def visitEo(self, ctx:AutomataParser.EoContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by AutomataParser#ll.
    def visitLl(self, ctx:AutomataParser.LlContext):
        return self.visitChildren(ctx)



del AutomataParser