# Generated from /home/kate/PycharmProjects/fl-2022-itmo-spr/src/grammar/MyGrammar.g4 by ANTLR 4.10.1
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO

def serializedATN():
    return [
        4,1,21,98,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
        6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,1,0,1,0,1,0,1,1,1,1,3,1,28,8,
        1,5,1,30,8,1,10,1,12,1,33,9,1,1,1,1,1,3,1,37,8,1,3,1,39,8,1,1,2,
        1,2,3,2,43,8,2,1,3,1,3,1,3,1,3,1,4,1,4,1,4,1,4,1,5,1,5,1,5,5,5,56,
        8,5,10,5,12,5,59,9,5,1,6,5,6,62,8,6,10,6,12,6,65,9,6,1,7,1,7,1,7,
        1,7,1,7,1,7,3,7,73,8,7,1,7,3,7,76,8,7,1,8,1,8,1,8,1,8,1,8,1,8,1,
        8,1,8,1,8,3,8,87,8,8,1,9,1,9,1,9,1,9,1,9,3,9,94,8,9,1,10,1,10,1,
        10,0,0,11,0,2,4,6,8,10,12,14,16,18,20,0,1,1,0,13,14,100,0,22,1,0,
        0,0,2,31,1,0,0,0,4,42,1,0,0,0,6,44,1,0,0,0,8,48,1,0,0,0,10,52,1,
        0,0,0,12,63,1,0,0,0,14,66,1,0,0,0,16,86,1,0,0,0,18,93,1,0,0,0,20,
        95,1,0,0,0,22,23,3,2,1,0,23,24,5,0,0,1,24,1,1,0,0,0,25,27,3,4,2,
        0,26,28,5,18,0,0,27,26,1,0,0,0,27,28,1,0,0,0,28,30,1,0,0,0,29,25,
        1,0,0,0,30,33,1,0,0,0,31,29,1,0,0,0,31,32,1,0,0,0,32,38,1,0,0,0,
        33,31,1,0,0,0,34,36,3,4,2,0,35,37,5,18,0,0,36,35,1,0,0,0,36,37,1,
        0,0,0,37,39,1,0,0,0,38,34,1,0,0,0,38,39,1,0,0,0,39,3,1,0,0,0,40,
        43,3,6,3,0,41,43,3,8,4,0,42,40,1,0,0,0,42,41,1,0,0,0,43,5,1,0,0,
        0,44,45,5,13,0,0,45,46,5,1,0,0,46,47,3,10,5,0,47,7,1,0,0,0,48,49,
        5,14,0,0,49,50,5,1,0,0,50,51,3,10,5,0,51,9,1,0,0,0,52,57,3,12,6,
        0,53,54,5,11,0,0,54,56,3,12,6,0,55,53,1,0,0,0,56,59,1,0,0,0,57,55,
        1,0,0,0,57,58,1,0,0,0,58,11,1,0,0,0,59,57,1,0,0,0,60,62,3,14,7,0,
        61,60,1,0,0,0,62,65,1,0,0,0,63,61,1,0,0,0,63,64,1,0,0,0,64,13,1,
        0,0,0,65,63,1,0,0,0,66,75,3,16,8,0,67,76,5,12,0,0,68,69,5,2,0,0,
        69,72,5,19,0,0,70,71,5,8,0,0,71,73,5,19,0,0,72,70,1,0,0,0,72,73,
        1,0,0,0,73,74,1,0,0,0,74,76,5,3,0,0,75,67,1,0,0,0,75,68,1,0,0,0,
        75,76,1,0,0,0,76,15,1,0,0,0,77,78,5,4,0,0,78,79,3,10,5,0,79,80,5,
        5,0,0,80,87,1,0,0,0,81,82,5,6,0,0,82,83,3,10,5,0,83,84,5,7,0,0,84,
        87,1,0,0,0,85,87,3,18,9,0,86,77,1,0,0,0,86,81,1,0,0,0,86,85,1,0,
        0,0,87,17,1,0,0,0,88,89,5,15,0,0,89,90,5,10,0,0,90,94,5,15,0,0,91,
        94,3,20,10,0,92,94,5,15,0,0,93,88,1,0,0,0,93,91,1,0,0,0,93,92,1,
        0,0,0,94,19,1,0,0,0,95,96,7,0,0,0,96,21,1,0,0,0,11,27,31,36,38,42,
        57,63,72,75,86,93
    ]

class MyGrammarParser ( Parser ):

    grammarFileName = "MyGrammar.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "':'", "'{'", "'}'", "'('", "')'", "'['", 
                     "']'", "','", "'.'", "'..'", "'|'" ]

    symbolicNames = [ "<INVALID>", "COLON", "LEFT_BRACE", "RIGHT_BRACE", 
                      "LEFT_PAREN", "RIGHT_PAREN", "LEFT_BRACKET", "RIGHT_BRACKET", 
                      "COMMA", "DOT", "DOUBLE_DOT", "VBAR", "OPERATOR", 
                      "RULE", "TOKEN", "STRING", "DOUBLE_QUOTED_STRING", 
                      "SINGLE_QUOTED_STRING", "NEW_LINE", "NUMBER", "WS", 
                      "COMMENT" ]

    RULE_startRule = 0
    RULE_block = 1
    RULE_item = 2
    RULE_ruleStatement = 3
    RULE_token = 4
    RULE_contents = 5
    RULE_content = 6
    RULE_expression = 7
    RULE_term = 8
    RULE_value = 9
    RULE_name = 10

    ruleNames =  [ "startRule", "block", "item", "ruleStatement", "token", 
                   "contents", "content", "expression", "term", "value", 
                   "name" ]

    EOF = Token.EOF
    COLON=1
    LEFT_BRACE=2
    RIGHT_BRACE=3
    LEFT_PAREN=4
    RIGHT_PAREN=5
    LEFT_BRACKET=6
    RIGHT_BRACKET=7
    COMMA=8
    DOT=9
    DOUBLE_DOT=10
    VBAR=11
    OPERATOR=12
    RULE=13
    TOKEN=14
    STRING=15
    DOUBLE_QUOTED_STRING=16
    SINGLE_QUOTED_STRING=17
    NEW_LINE=18
    NUMBER=19
    WS=20
    COMMENT=21

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.10.1")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class StartRuleContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def block(self):
            return self.getTypedRuleContext(MyGrammarParser.BlockContext,0)


        def EOF(self):
            return self.getToken(MyGrammarParser.EOF, 0)

        def getRuleIndex(self):
            return MyGrammarParser.RULE_startRule

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterStartRule" ):
                listener.enterStartRule(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitStartRule" ):
                listener.exitStartRule(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitStartRule" ):
                return visitor.visitStartRule(self)
            else:
                return visitor.visitChildren(self)




    def startRule(self):

        localctx = MyGrammarParser.StartRuleContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_startRule)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 22
            self.block()
            self.state = 23
            self.match(MyGrammarParser.EOF)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class BlockContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def item(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MyGrammarParser.ItemContext)
            else:
                return self.getTypedRuleContext(MyGrammarParser.ItemContext,i)


        def NEW_LINE(self, i:int=None):
            if i is None:
                return self.getTokens(MyGrammarParser.NEW_LINE)
            else:
                return self.getToken(MyGrammarParser.NEW_LINE, i)

        def getRuleIndex(self):
            return MyGrammarParser.RULE_block

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterBlock" ):
                listener.enterBlock(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitBlock" ):
                listener.exitBlock(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitBlock" ):
                return visitor.visitBlock(self)
            else:
                return visitor.visitChildren(self)




    def block(self):

        localctx = MyGrammarParser.BlockContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_block)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 31
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,1,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 25
                    self.item()
                    self.state = 27
                    self._errHandler.sync(self)
                    _la = self._input.LA(1)
                    if _la==MyGrammarParser.NEW_LINE:
                        self.state = 26
                        self.match(MyGrammarParser.NEW_LINE)

             
                self.state = 33
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,1,self._ctx)

            self.state = 38
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            if _la==MyGrammarParser.RULE or _la==MyGrammarParser.TOKEN:
                self.state = 34
                self.item()
                self.state = 36
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==MyGrammarParser.NEW_LINE:
                    self.state = 35
                    self.match(MyGrammarParser.NEW_LINE)




        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ItemContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def ruleStatement(self):
            return self.getTypedRuleContext(MyGrammarParser.RuleStatementContext,0)


        def token(self):
            return self.getTypedRuleContext(MyGrammarParser.TokenContext,0)


        def getRuleIndex(self):
            return MyGrammarParser.RULE_item

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterItem" ):
                listener.enterItem(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitItem" ):
                listener.exitItem(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitItem" ):
                return visitor.visitItem(self)
            else:
                return visitor.visitChildren(self)




    def item(self):

        localctx = MyGrammarParser.ItemContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_item)
        try:
            self.state = 42
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [MyGrammarParser.RULE]:
                self.enterOuterAlt(localctx, 1)
                self.state = 40
                self.ruleStatement()
                pass
            elif token in [MyGrammarParser.TOKEN]:
                self.enterOuterAlt(localctx, 2)
                self.state = 41
                self.token()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class RuleStatementContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def RULE(self):
            return self.getToken(MyGrammarParser.RULE, 0)

        def COLON(self):
            return self.getToken(MyGrammarParser.COLON, 0)

        def contents(self):
            return self.getTypedRuleContext(MyGrammarParser.ContentsContext,0)


        def getRuleIndex(self):
            return MyGrammarParser.RULE_ruleStatement

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterRuleStatement" ):
                listener.enterRuleStatement(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitRuleStatement" ):
                listener.exitRuleStatement(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitRuleStatement" ):
                return visitor.visitRuleStatement(self)
            else:
                return visitor.visitChildren(self)




    def ruleStatement(self):

        localctx = MyGrammarParser.RuleStatementContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_ruleStatement)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 44
            self.match(MyGrammarParser.RULE)
            self.state = 45
            self.match(MyGrammarParser.COLON)
            self.state = 46
            self.contents()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TokenContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def TOKEN(self):
            return self.getToken(MyGrammarParser.TOKEN, 0)

        def COLON(self):
            return self.getToken(MyGrammarParser.COLON, 0)

        def contents(self):
            return self.getTypedRuleContext(MyGrammarParser.ContentsContext,0)


        def getRuleIndex(self):
            return MyGrammarParser.RULE_token

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterToken" ):
                listener.enterToken(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitToken" ):
                listener.exitToken(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitToken" ):
                return visitor.visitToken(self)
            else:
                return visitor.visitChildren(self)




    def token(self):

        localctx = MyGrammarParser.TokenContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_token)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 48
            self.match(MyGrammarParser.TOKEN)
            self.state = 49
            self.match(MyGrammarParser.COLON)
            self.state = 50
            self.contents()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ContentsContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def content(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MyGrammarParser.ContentContext)
            else:
                return self.getTypedRuleContext(MyGrammarParser.ContentContext,i)


        def VBAR(self, i:int=None):
            if i is None:
                return self.getTokens(MyGrammarParser.VBAR)
            else:
                return self.getToken(MyGrammarParser.VBAR, i)

        def getRuleIndex(self):
            return MyGrammarParser.RULE_contents

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterContents" ):
                listener.enterContents(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitContents" ):
                listener.exitContents(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitContents" ):
                return visitor.visitContents(self)
            else:
                return visitor.visitChildren(self)




    def contents(self):

        localctx = MyGrammarParser.ContentsContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_contents)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 52
            self.content()
            self.state = 57
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==MyGrammarParser.VBAR:
                self.state = 53
                self.match(MyGrammarParser.VBAR)
                self.state = 54
                self.content()
                self.state = 59
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ContentContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expression(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(MyGrammarParser.ExpressionContext)
            else:
                return self.getTypedRuleContext(MyGrammarParser.ExpressionContext,i)


        def getRuleIndex(self):
            return MyGrammarParser.RULE_content

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterContent" ):
                listener.enterContent(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitContent" ):
                listener.exitContent(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitContent" ):
                return visitor.visitContent(self)
            else:
                return visitor.visitChildren(self)




    def content(self):

        localctx = MyGrammarParser.ContentContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_content)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 63
            self._errHandler.sync(self)
            _alt = self._interp.adaptivePredict(self._input,6,self._ctx)
            while _alt!=2 and _alt!=ATN.INVALID_ALT_NUMBER:
                if _alt==1:
                    self.state = 60
                    self.expression() 
                self.state = 65
                self._errHandler.sync(self)
                _alt = self._interp.adaptivePredict(self._input,6,self._ctx)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExpressionContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def term(self):
            return self.getTypedRuleContext(MyGrammarParser.TermContext,0)


        def OPERATOR(self):
            return self.getToken(MyGrammarParser.OPERATOR, 0)

        def LEFT_BRACE(self):
            return self.getToken(MyGrammarParser.LEFT_BRACE, 0)

        def NUMBER(self, i:int=None):
            if i is None:
                return self.getTokens(MyGrammarParser.NUMBER)
            else:
                return self.getToken(MyGrammarParser.NUMBER, i)

        def RIGHT_BRACE(self):
            return self.getToken(MyGrammarParser.RIGHT_BRACE, 0)

        def COMMA(self):
            return self.getToken(MyGrammarParser.COMMA, 0)

        def getRuleIndex(self):
            return MyGrammarParser.RULE_expression

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterExpression" ):
                listener.enterExpression(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitExpression" ):
                listener.exitExpression(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitExpression" ):
                return visitor.visitExpression(self)
            else:
                return visitor.visitChildren(self)




    def expression(self):

        localctx = MyGrammarParser.ExpressionContext(self, self._ctx, self.state)
        self.enterRule(localctx, 14, self.RULE_expression)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 66
            self.term()
            self.state = 75
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [MyGrammarParser.OPERATOR]:
                self.state = 67
                self.match(MyGrammarParser.OPERATOR)
                pass
            elif token in [MyGrammarParser.LEFT_BRACE]:
                self.state = 68
                self.match(MyGrammarParser.LEFT_BRACE)
                self.state = 69
                self.match(MyGrammarParser.NUMBER)
                self.state = 72
                self._errHandler.sync(self)
                _la = self._input.LA(1)
                if _la==MyGrammarParser.COMMA:
                    self.state = 70
                    self.match(MyGrammarParser.COMMA)
                    self.state = 71
                    self.match(MyGrammarParser.NUMBER)


                self.state = 74
                self.match(MyGrammarParser.RIGHT_BRACE)
                pass
            elif token in [MyGrammarParser.EOF, MyGrammarParser.LEFT_PAREN, MyGrammarParser.RIGHT_PAREN, MyGrammarParser.LEFT_BRACKET, MyGrammarParser.RIGHT_BRACKET, MyGrammarParser.VBAR, MyGrammarParser.RULE, MyGrammarParser.TOKEN, MyGrammarParser.STRING, MyGrammarParser.NEW_LINE]:
                pass
            else:
                pass
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TermContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def LEFT_PAREN(self):
            return self.getToken(MyGrammarParser.LEFT_PAREN, 0)

        def contents(self):
            return self.getTypedRuleContext(MyGrammarParser.ContentsContext,0)


        def RIGHT_PAREN(self):
            return self.getToken(MyGrammarParser.RIGHT_PAREN, 0)

        def LEFT_BRACKET(self):
            return self.getToken(MyGrammarParser.LEFT_BRACKET, 0)

        def RIGHT_BRACKET(self):
            return self.getToken(MyGrammarParser.RIGHT_BRACKET, 0)

        def value(self):
            return self.getTypedRuleContext(MyGrammarParser.ValueContext,0)


        def getRuleIndex(self):
            return MyGrammarParser.RULE_term

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterTerm" ):
                listener.enterTerm(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitTerm" ):
                listener.exitTerm(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitTerm" ):
                return visitor.visitTerm(self)
            else:
                return visitor.visitChildren(self)




    def term(self):

        localctx = MyGrammarParser.TermContext(self, self._ctx, self.state)
        self.enterRule(localctx, 16, self.RULE_term)
        try:
            self.state = 86
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [MyGrammarParser.LEFT_PAREN]:
                self.enterOuterAlt(localctx, 1)
                self.state = 77
                self.match(MyGrammarParser.LEFT_PAREN)
                self.state = 78
                self.contents()
                self.state = 79
                self.match(MyGrammarParser.RIGHT_PAREN)
                pass
            elif token in [MyGrammarParser.LEFT_BRACKET]:
                self.enterOuterAlt(localctx, 2)
                self.state = 81
                self.match(MyGrammarParser.LEFT_BRACKET)
                self.state = 82
                self.contents()
                self.state = 83
                self.match(MyGrammarParser.RIGHT_BRACKET)
                pass
            elif token in [MyGrammarParser.RULE, MyGrammarParser.TOKEN, MyGrammarParser.STRING]:
                self.enterOuterAlt(localctx, 3)
                self.state = 85
                self.value()
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ValueContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def STRING(self, i:int=None):
            if i is None:
                return self.getTokens(MyGrammarParser.STRING)
            else:
                return self.getToken(MyGrammarParser.STRING, i)

        def DOUBLE_DOT(self):
            return self.getToken(MyGrammarParser.DOUBLE_DOT, 0)

        def name(self):
            return self.getTypedRuleContext(MyGrammarParser.NameContext,0)


        def getRuleIndex(self):
            return MyGrammarParser.RULE_value

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterValue" ):
                listener.enterValue(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitValue" ):
                listener.exitValue(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitValue" ):
                return visitor.visitValue(self)
            else:
                return visitor.visitChildren(self)




    def value(self):

        localctx = MyGrammarParser.ValueContext(self, self._ctx, self.state)
        self.enterRule(localctx, 18, self.RULE_value)
        try:
            self.state = 93
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,10,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 88
                self.match(MyGrammarParser.STRING)
                self.state = 89
                self.match(MyGrammarParser.DOUBLE_DOT)
                self.state = 90
                self.match(MyGrammarParser.STRING)
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 91
                self.name()
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)
                self.state = 92
                self.match(MyGrammarParser.STRING)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class NameContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def RULE(self):
            return self.getToken(MyGrammarParser.RULE, 0)

        def TOKEN(self):
            return self.getToken(MyGrammarParser.TOKEN, 0)

        def getRuleIndex(self):
            return MyGrammarParser.RULE_name

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterName" ):
                listener.enterName(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitName" ):
                listener.exitName(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitName" ):
                return visitor.visitName(self)
            else:
                return visitor.visitChildren(self)




    def name(self):

        localctx = MyGrammarParser.NameContext(self, self._ctx, self.state)
        self.enterRule(localctx, 20, self.RULE_name)
        self._la = 0 # Token type
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 95
            _la = self._input.LA(1)
            if not(_la==MyGrammarParser.RULE or _la==MyGrammarParser.TOKEN):
                self._errHandler.recoverInline(self)
            else:
                self._errHandler.reportMatch(self)
                self.consume()
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





