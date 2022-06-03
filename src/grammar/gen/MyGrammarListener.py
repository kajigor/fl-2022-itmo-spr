# Generated from /home/kate/PycharmProjects/fl-2022-itmo-spr/src/grammar/MyGrammar.g4 by ANTLR 4.10.1
from antlr4 import *
if __name__ is not None and "." in __name__:
    from .MyGrammarParser import MyGrammarParser
else:
    from MyGrammarParser import MyGrammarParser

# This class defines a complete listener for a parse tree produced by MyGrammarParser.
class MyGrammarListener(ParseTreeListener):

    # Enter a parse tree produced by MyGrammarParser#startRule.
    def enterStartRule(self, ctx:MyGrammarParser.StartRuleContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#startRule.
    def exitStartRule(self, ctx:MyGrammarParser.StartRuleContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#block.
    def enterBlock(self, ctx:MyGrammarParser.BlockContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#block.
    def exitBlock(self, ctx:MyGrammarParser.BlockContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#item.
    def enterItem(self, ctx:MyGrammarParser.ItemContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#item.
    def exitItem(self, ctx:MyGrammarParser.ItemContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#ruleStatement.
    def enterRuleStatement(self, ctx:MyGrammarParser.RuleStatementContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#ruleStatement.
    def exitRuleStatement(self, ctx:MyGrammarParser.RuleStatementContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#token.
    def enterToken(self, ctx:MyGrammarParser.TokenContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#token.
    def exitToken(self, ctx:MyGrammarParser.TokenContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#contents.
    def enterContents(self, ctx:MyGrammarParser.ContentsContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#contents.
    def exitContents(self, ctx:MyGrammarParser.ContentsContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#content.
    def enterContent(self, ctx:MyGrammarParser.ContentContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#content.
    def exitContent(self, ctx:MyGrammarParser.ContentContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#expression.
    def enterExpression(self, ctx:MyGrammarParser.ExpressionContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#expression.
    def exitExpression(self, ctx:MyGrammarParser.ExpressionContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#term.
    def enterTerm(self, ctx:MyGrammarParser.TermContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#term.
    def exitTerm(self, ctx:MyGrammarParser.TermContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#value.
    def enterValue(self, ctx:MyGrammarParser.ValueContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#value.
    def exitValue(self, ctx:MyGrammarParser.ValueContext):
        pass


    # Enter a parse tree produced by MyGrammarParser#name.
    def enterName(self, ctx:MyGrammarParser.NameContext):
        pass

    # Exit a parse tree produced by MyGrammarParser#name.
    def exitName(self, ctx:MyGrammarParser.NameContext):
        pass



del MyGrammarParser