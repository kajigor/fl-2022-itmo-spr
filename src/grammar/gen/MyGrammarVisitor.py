# Generated from /home/kate/PycharmProjects/fl-2022-itmo-spr/src/grammar/MyGrammar.g4 by ANTLR 4.10.1
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .MyGrammarParser import MyGrammarParser
else:
    from MyGrammarParser import MyGrammarParser

# This class defines a complete generic visitor for a parse tree produced by MyGrammarParser.

class MyGrammarVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by MyGrammarParser#startRule.
    def visitStartRule(self, ctx:MyGrammarParser.StartRuleContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MyGrammarParser#item.
    def visitItem(self, ctx:MyGrammarParser.ItemContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MyGrammarParser#ruleStatement.
    def visitRuleStatement(self, ctx:MyGrammarParser.RuleStatementContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MyGrammarParser#token.
    def visitToken(self, ctx:MyGrammarParser.TokenContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MyGrammarParser#contents.
    def visitContents(self, ctx:MyGrammarParser.ContentsContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MyGrammarParser#content.
    def visitContent(self, ctx:MyGrammarParser.ContentContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MyGrammarParser#expression.
    def visitExpression(self, ctx:MyGrammarParser.ExpressionContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MyGrammarParser#term.
    def visitTerm(self, ctx:MyGrammarParser.TermContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MyGrammarParser#value.
    def visitValue(self, ctx:MyGrammarParser.ValueContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by MyGrammarParser#name.
    def visitName(self, ctx:MyGrammarParser.NameContext):
        return self.visitChildren(ctx)



del MyGrammarParser