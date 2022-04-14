# Generated from Automata.g4 by ANTLR 4.10
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
        4,1,18,197,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
        6,2,7,7,7,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,1,57,8,1,1,2,1,2,1,2,
        1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,
        1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,
        1,2,1,2,1,2,1,2,1,2,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,
        1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,121,8,3,1,4,1,4,
        1,4,1,4,1,4,1,4,1,4,1,4,1,4,3,4,132,8,4,1,5,1,5,1,5,1,5,1,5,1,5,
        1,5,1,5,1,5,1,5,1,5,1,5,1,5,1,5,1,5,1,5,1,5,1,5,3,5,152,8,5,1,6,
        1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,6,
        1,6,3,6,172,8,6,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,
        1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,1,7,3,7,195,8,7,1,7,0,0,8,0,2,4,
        6,8,10,12,14,0,0,200,0,16,1,0,0,0,2,56,1,0,0,0,4,58,1,0,0,0,6,120,
        1,0,0,0,8,131,1,0,0,0,10,151,1,0,0,0,12,171,1,0,0,0,14,194,1,0,0,
        0,16,17,5,1,0,0,17,18,3,2,1,0,18,19,5,2,0,0,19,1,1,0,0,0,20,21,5,
        3,0,0,21,22,5,17,0,0,22,23,5,3,0,0,23,24,5,4,0,0,24,25,5,1,0,0,25,
        26,3,4,2,0,26,27,5,2,0,0,27,28,5,5,0,0,28,29,3,2,1,0,29,57,1,0,0,
        0,30,31,5,3,0,0,31,32,5,17,0,0,32,33,5,3,0,0,33,34,5,4,0,0,34,35,
        5,1,0,0,35,36,3,4,2,0,36,37,5,2,0,0,37,57,1,0,0,0,38,39,5,3,0,0,
        39,40,5,17,0,0,40,41,5,3,0,0,41,42,5,4,0,0,42,43,5,6,0,0,43,44,3,
        6,3,0,44,45,5,7,0,0,45,46,5,5,0,0,46,47,3,2,1,0,47,57,1,0,0,0,48,
        49,5,3,0,0,49,50,5,17,0,0,50,51,5,3,0,0,51,52,5,4,0,0,52,53,5,6,
        0,0,53,54,3,6,3,0,54,55,5,7,0,0,55,57,1,0,0,0,56,20,1,0,0,0,56,30,
        1,0,0,0,56,38,1,0,0,0,56,48,1,0,0,0,57,3,1,0,0,0,58,59,5,8,0,0,59,
        60,5,4,0,0,60,61,5,6,0,0,61,62,3,8,4,0,62,63,5,7,0,0,63,64,5,5,0,
        0,64,65,5,9,0,0,65,66,5,4,0,0,66,67,5,6,0,0,67,68,3,8,4,0,68,69,
        5,7,0,0,69,70,5,5,0,0,70,71,5,10,0,0,71,72,5,4,0,0,72,73,5,3,0,0,
        73,74,5,17,0,0,74,75,5,3,0,0,75,76,5,5,0,0,76,77,5,11,0,0,77,78,
        5,4,0,0,78,79,5,6,0,0,79,80,3,8,4,0,80,81,5,7,0,0,81,82,5,5,0,0,
        82,83,5,12,0,0,83,84,5,4,0,0,84,85,5,16,0,0,85,86,5,5,0,0,86,87,
        5,13,0,0,87,88,5,4,0,0,88,89,5,1,0,0,89,90,3,12,6,0,90,91,5,2,0,
        0,91,92,5,5,0,0,92,93,5,14,0,0,93,94,5,4,0,0,94,95,5,1,0,0,95,96,
        3,10,5,0,96,97,5,2,0,0,97,5,1,0,0,0,98,99,5,1,0,0,99,100,3,4,2,0,
        100,101,5,2,0,0,101,102,5,5,0,0,102,103,5,15,0,0,103,104,5,5,0,0,
        104,105,3,6,3,0,105,121,1,0,0,0,106,107,5,3,0,0,107,108,5,17,0,0,
        108,109,5,3,0,0,109,110,5,5,0,0,110,111,5,15,0,0,111,112,5,5,0,0,
        112,121,3,6,3,0,113,114,5,1,0,0,114,115,3,4,2,0,115,116,5,2,0,0,
        116,121,1,0,0,0,117,118,5,3,0,0,118,119,5,17,0,0,119,121,5,3,0,0,
        120,98,1,0,0,0,120,106,1,0,0,0,120,113,1,0,0,0,120,117,1,0,0,0,121,
        7,1,0,0,0,122,123,5,3,0,0,123,124,5,17,0,0,124,125,5,3,0,0,125,126,
        5,5,0,0,126,132,3,8,4,0,127,128,5,3,0,0,128,129,5,17,0,0,129,132,
        5,3,0,0,130,132,1,0,0,0,131,122,1,0,0,0,131,127,1,0,0,0,131,130,
        1,0,0,0,132,9,1,0,0,0,133,134,5,3,0,0,134,135,5,17,0,0,135,136,5,
        3,0,0,136,137,5,4,0,0,137,138,5,6,0,0,138,139,3,8,4,0,139,140,5,
        7,0,0,140,141,5,5,0,0,141,142,3,10,5,0,142,152,1,0,0,0,143,144,5,
        3,0,0,144,145,5,17,0,0,145,146,5,3,0,0,146,147,5,4,0,0,147,148,5,
        6,0,0,148,149,3,8,4,0,149,150,5,7,0,0,150,152,1,0,0,0,151,133,1,
        0,0,0,151,143,1,0,0,0,152,11,1,0,0,0,153,154,5,3,0,0,154,155,5,17,
        0,0,155,156,5,3,0,0,156,157,5,4,0,0,157,158,5,6,0,0,158,159,3,14,
        7,0,159,160,5,7,0,0,160,161,5,5,0,0,161,162,3,12,6,0,162,172,1,0,
        0,0,163,164,5,3,0,0,164,165,5,17,0,0,165,166,5,3,0,0,166,167,5,4,
        0,0,167,168,5,6,0,0,168,169,3,14,7,0,169,170,5,7,0,0,170,172,1,0,
        0,0,171,153,1,0,0,0,171,163,1,0,0,0,172,13,1,0,0,0,173,174,5,6,0,
        0,174,175,5,3,0,0,175,176,5,17,0,0,176,177,5,3,0,0,177,178,5,5,0,
        0,178,179,5,3,0,0,179,180,5,17,0,0,180,181,5,3,0,0,181,182,5,7,0,
        0,182,183,5,5,0,0,183,195,3,14,7,0,184,185,5,6,0,0,185,186,5,3,0,
        0,186,187,5,17,0,0,187,188,5,3,0,0,188,189,5,5,0,0,189,190,5,3,0,
        0,190,191,5,17,0,0,191,192,5,3,0,0,192,195,5,7,0,0,193,195,1,0,0,
        0,194,173,1,0,0,0,194,184,1,0,0,0,194,193,1,0,0,0,195,15,1,0,0,0,
        6,56,120,131,151,171,194
    ]

class AutomataParser ( Parser ):

    grammarFileName = "Automata.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'{'", "'}'", "'\"'", "':'", "','", "'['", 
                     "']'", "'\"glossary\"'", "'\"states\"'", "'\"initial_state\"'", 
                     "'\"terminal_states\"'", "'\"is_dfa\"'", "'\"edges\"'", 
                     "'\"edges_epsilon\"'" ]

    symbolicNames = [ "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                      "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                      "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                      "<INVALID>", "<INVALID>", "<INVALID>", "O", "B", "C", 
                      "WS" ]

    RULE_s = 0
    RULE_lp = 1
    RULE_a = 2
    RULE_lao = 3
    RULE_lc = 4
    RULE_ee = 5
    RULE_eo = 6
    RULE_ll = 7

    ruleNames =  [ "s", "lp", "a", "lao", "lc", "ee", "eo", "ll" ]

    EOF = Token.EOF
    T__0=1
    T__1=2
    T__2=3
    T__3=4
    T__4=5
    T__5=6
    T__6=7
    T__7=8
    T__8=9
    T__9=10
    T__10=11
    T__11=12
    T__12=13
    T__13=14
    O=15
    B=16
    C=17
    WS=18

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.10")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None




    class SContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def lp(self):
            return self.getTypedRuleContext(AutomataParser.LpContext,0)


        def getRuleIndex(self):
            return AutomataParser.RULE_s

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterS" ):
                listener.enterS(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitS" ):
                listener.exitS(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitS" ):
                return visitor.visitS(self)
            else:
                return visitor.visitChildren(self)




    def s(self):

        localctx = AutomataParser.SContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_s)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 16
            self.match(AutomataParser.T__0)
            self.state = 17
            self.lp()
            self.state = 18
            self.match(AutomataParser.T__1)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LpContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def C(self):
            return self.getToken(AutomataParser.C, 0)

        def a(self):
            return self.getTypedRuleContext(AutomataParser.AContext,0)


        def lp(self):
            return self.getTypedRuleContext(AutomataParser.LpContext,0)


        def lao(self):
            return self.getTypedRuleContext(AutomataParser.LaoContext,0)


        def getRuleIndex(self):
            return AutomataParser.RULE_lp

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterLp" ):
                listener.enterLp(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitLp" ):
                listener.exitLp(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitLp" ):
                return visitor.visitLp(self)
            else:
                return visitor.visitChildren(self)




    def lp(self):

        localctx = AutomataParser.LpContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_lp)
        try:
            self.state = 56
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,0,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 20
                self.match(AutomataParser.T__2)
                self.state = 21
                self.match(AutomataParser.C)
                self.state = 22
                self.match(AutomataParser.T__2)
                self.state = 23
                self.match(AutomataParser.T__3)
                self.state = 24
                self.match(AutomataParser.T__0)
                self.state = 25
                self.a()
                self.state = 26
                self.match(AutomataParser.T__1)
                self.state = 27
                self.match(AutomataParser.T__4)
                self.state = 28
                self.lp()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 30
                self.match(AutomataParser.T__2)
                self.state = 31
                self.match(AutomataParser.C)
                self.state = 32
                self.match(AutomataParser.T__2)
                self.state = 33
                self.match(AutomataParser.T__3)
                self.state = 34
                self.match(AutomataParser.T__0)
                self.state = 35
                self.a()
                self.state = 36
                self.match(AutomataParser.T__1)
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)
                self.state = 38
                self.match(AutomataParser.T__2)
                self.state = 39
                self.match(AutomataParser.C)
                self.state = 40
                self.match(AutomataParser.T__2)
                self.state = 41
                self.match(AutomataParser.T__3)
                self.state = 42
                self.match(AutomataParser.T__5)
                self.state = 43
                self.lao()
                self.state = 44
                self.match(AutomataParser.T__6)
                self.state = 45
                self.match(AutomataParser.T__4)
                self.state = 46
                self.lp()
                pass

            elif la_ == 4:
                self.enterOuterAlt(localctx, 4)
                self.state = 48
                self.match(AutomataParser.T__2)
                self.state = 49
                self.match(AutomataParser.C)
                self.state = 50
                self.match(AutomataParser.T__2)
                self.state = 51
                self.match(AutomataParser.T__3)
                self.state = 52
                self.match(AutomataParser.T__5)
                self.state = 53
                self.lao()
                self.state = 54
                self.match(AutomataParser.T__6)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class AContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def lc(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(AutomataParser.LcContext)
            else:
                return self.getTypedRuleContext(AutomataParser.LcContext,i)


        def C(self):
            return self.getToken(AutomataParser.C, 0)

        def B(self):
            return self.getToken(AutomataParser.B, 0)

        def eo(self):
            return self.getTypedRuleContext(AutomataParser.EoContext,0)


        def ee(self):
            return self.getTypedRuleContext(AutomataParser.EeContext,0)


        def getRuleIndex(self):
            return AutomataParser.RULE_a

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterA" ):
                listener.enterA(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitA" ):
                listener.exitA(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitA" ):
                return visitor.visitA(self)
            else:
                return visitor.visitChildren(self)




    def a(self):

        localctx = AutomataParser.AContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_a)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 58
            self.match(AutomataParser.T__7)
            self.state = 59
            self.match(AutomataParser.T__3)
            self.state = 60
            self.match(AutomataParser.T__5)
            self.state = 61
            self.lc()
            self.state = 62
            self.match(AutomataParser.T__6)
            self.state = 63
            self.match(AutomataParser.T__4)
            self.state = 64
            self.match(AutomataParser.T__8)
            self.state = 65
            self.match(AutomataParser.T__3)
            self.state = 66
            self.match(AutomataParser.T__5)
            self.state = 67
            self.lc()
            self.state = 68
            self.match(AutomataParser.T__6)
            self.state = 69
            self.match(AutomataParser.T__4)
            self.state = 70
            self.match(AutomataParser.T__9)
            self.state = 71
            self.match(AutomataParser.T__3)
            self.state = 72
            self.match(AutomataParser.T__2)
            self.state = 73
            self.match(AutomataParser.C)
            self.state = 74
            self.match(AutomataParser.T__2)
            self.state = 75
            self.match(AutomataParser.T__4)
            self.state = 76
            self.match(AutomataParser.T__10)
            self.state = 77
            self.match(AutomataParser.T__3)
            self.state = 78
            self.match(AutomataParser.T__5)
            self.state = 79
            self.lc()
            self.state = 80
            self.match(AutomataParser.T__6)
            self.state = 81
            self.match(AutomataParser.T__4)
            self.state = 82
            self.match(AutomataParser.T__11)
            self.state = 83
            self.match(AutomataParser.T__3)
            self.state = 84
            self.match(AutomataParser.B)
            self.state = 85
            self.match(AutomataParser.T__4)
            self.state = 86
            self.match(AutomataParser.T__12)
            self.state = 87
            self.match(AutomataParser.T__3)
            self.state = 88
            self.match(AutomataParser.T__0)
            self.state = 89
            self.eo()
            self.state = 90
            self.match(AutomataParser.T__1)
            self.state = 91
            self.match(AutomataParser.T__4)
            self.state = 92
            self.match(AutomataParser.T__13)
            self.state = 93
            self.match(AutomataParser.T__3)
            self.state = 94
            self.match(AutomataParser.T__0)
            self.state = 95
            self.ee()
            self.state = 96
            self.match(AutomataParser.T__1)
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LaoContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def a(self):
            return self.getTypedRuleContext(AutomataParser.AContext,0)


        def O(self):
            return self.getToken(AutomataParser.O, 0)

        def lao(self):
            return self.getTypedRuleContext(AutomataParser.LaoContext,0)


        def C(self):
            return self.getToken(AutomataParser.C, 0)

        def getRuleIndex(self):
            return AutomataParser.RULE_lao

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterLao" ):
                listener.enterLao(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitLao" ):
                listener.exitLao(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitLao" ):
                return visitor.visitLao(self)
            else:
                return visitor.visitChildren(self)




    def lao(self):

        localctx = AutomataParser.LaoContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_lao)
        try:
            self.state = 120
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,1,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 98
                self.match(AutomataParser.T__0)
                self.state = 99
                self.a()
                self.state = 100
                self.match(AutomataParser.T__1)
                self.state = 101
                self.match(AutomataParser.T__4)
                self.state = 102
                self.match(AutomataParser.O)
                self.state = 103
                self.match(AutomataParser.T__4)
                self.state = 104
                self.lao()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 106
                self.match(AutomataParser.T__2)
                self.state = 107
                self.match(AutomataParser.C)
                self.state = 108
                self.match(AutomataParser.T__2)
                self.state = 109
                self.match(AutomataParser.T__4)
                self.state = 110
                self.match(AutomataParser.O)
                self.state = 111
                self.match(AutomataParser.T__4)
                self.state = 112
                self.lao()
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)
                self.state = 113
                self.match(AutomataParser.T__0)
                self.state = 114
                self.a()
                self.state = 115
                self.match(AutomataParser.T__1)
                pass

            elif la_ == 4:
                self.enterOuterAlt(localctx, 4)
                self.state = 117
                self.match(AutomataParser.T__2)
                self.state = 118
                self.match(AutomataParser.C)
                self.state = 119
                self.match(AutomataParser.T__2)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LcContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def C(self):
            return self.getToken(AutomataParser.C, 0)

        def lc(self):
            return self.getTypedRuleContext(AutomataParser.LcContext,0)


        def getRuleIndex(self):
            return AutomataParser.RULE_lc

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterLc" ):
                listener.enterLc(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitLc" ):
                listener.exitLc(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitLc" ):
                return visitor.visitLc(self)
            else:
                return visitor.visitChildren(self)




    def lc(self):

        localctx = AutomataParser.LcContext(self, self._ctx, self.state)
        self.enterRule(localctx, 8, self.RULE_lc)
        try:
            self.state = 131
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,2,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 122
                self.match(AutomataParser.T__2)
                self.state = 123
                self.match(AutomataParser.C)
                self.state = 124
                self.match(AutomataParser.T__2)
                self.state = 125
                self.match(AutomataParser.T__4)
                self.state = 126
                self.lc()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 127
                self.match(AutomataParser.T__2)
                self.state = 128
                self.match(AutomataParser.C)
                self.state = 129
                self.match(AutomataParser.T__2)
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)

                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class EeContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def C(self):
            return self.getToken(AutomataParser.C, 0)

        def lc(self):
            return self.getTypedRuleContext(AutomataParser.LcContext,0)


        def ee(self):
            return self.getTypedRuleContext(AutomataParser.EeContext,0)


        def getRuleIndex(self):
            return AutomataParser.RULE_ee

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterEe" ):
                listener.enterEe(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitEe" ):
                listener.exitEe(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitEe" ):
                return visitor.visitEe(self)
            else:
                return visitor.visitChildren(self)




    def ee(self):

        localctx = AutomataParser.EeContext(self, self._ctx, self.state)
        self.enterRule(localctx, 10, self.RULE_ee)
        try:
            self.state = 151
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,3,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 133
                self.match(AutomataParser.T__2)
                self.state = 134
                self.match(AutomataParser.C)
                self.state = 135
                self.match(AutomataParser.T__2)
                self.state = 136
                self.match(AutomataParser.T__3)
                self.state = 137
                self.match(AutomataParser.T__5)
                self.state = 138
                self.lc()
                self.state = 139
                self.match(AutomataParser.T__6)
                self.state = 140
                self.match(AutomataParser.T__4)
                self.state = 141
                self.ee()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 143
                self.match(AutomataParser.T__2)
                self.state = 144
                self.match(AutomataParser.C)
                self.state = 145
                self.match(AutomataParser.T__2)
                self.state = 146
                self.match(AutomataParser.T__3)
                self.state = 147
                self.match(AutomataParser.T__5)
                self.state = 148
                self.lc()
                self.state = 149
                self.match(AutomataParser.T__6)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class EoContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def C(self):
            return self.getToken(AutomataParser.C, 0)

        def ll(self):
            return self.getTypedRuleContext(AutomataParser.LlContext,0)


        def eo(self):
            return self.getTypedRuleContext(AutomataParser.EoContext,0)


        def getRuleIndex(self):
            return AutomataParser.RULE_eo

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterEo" ):
                listener.enterEo(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitEo" ):
                listener.exitEo(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitEo" ):
                return visitor.visitEo(self)
            else:
                return visitor.visitChildren(self)




    def eo(self):

        localctx = AutomataParser.EoContext(self, self._ctx, self.state)
        self.enterRule(localctx, 12, self.RULE_eo)
        try:
            self.state = 171
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,4,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 153
                self.match(AutomataParser.T__2)
                self.state = 154
                self.match(AutomataParser.C)
                self.state = 155
                self.match(AutomataParser.T__2)
                self.state = 156
                self.match(AutomataParser.T__3)
                self.state = 157
                self.match(AutomataParser.T__5)
                self.state = 158
                self.ll()
                self.state = 159
                self.match(AutomataParser.T__6)
                self.state = 160
                self.match(AutomataParser.T__4)
                self.state = 161
                self.eo()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 163
                self.match(AutomataParser.T__2)
                self.state = 164
                self.match(AutomataParser.C)
                self.state = 165
                self.match(AutomataParser.T__2)
                self.state = 166
                self.match(AutomataParser.T__3)
                self.state = 167
                self.match(AutomataParser.T__5)
                self.state = 168
                self.ll()
                self.state = 169
                self.match(AutomataParser.T__6)
                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class LlContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def C(self, i:int=None):
            if i is None:
                return self.getTokens(AutomataParser.C)
            else:
                return self.getToken(AutomataParser.C, i)

        def ll(self):
            return self.getTypedRuleContext(AutomataParser.LlContext,0)


        def getRuleIndex(self):
            return AutomataParser.RULE_ll

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterLl" ):
                listener.enterLl(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitLl" ):
                listener.exitLl(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitLl" ):
                return visitor.visitLl(self)
            else:
                return visitor.visitChildren(self)




    def ll(self):

        localctx = AutomataParser.LlContext(self, self._ctx, self.state)
        self.enterRule(localctx, 14, self.RULE_ll)
        try:
            self.state = 194
            self._errHandler.sync(self)
            la_ = self._interp.adaptivePredict(self._input,5,self._ctx)
            if la_ == 1:
                self.enterOuterAlt(localctx, 1)
                self.state = 173
                self.match(AutomataParser.T__5)
                self.state = 174
                self.match(AutomataParser.T__2)
                self.state = 175
                self.match(AutomataParser.C)
                self.state = 176
                self.match(AutomataParser.T__2)
                self.state = 177
                self.match(AutomataParser.T__4)
                self.state = 178
                self.match(AutomataParser.T__2)
                self.state = 179
                self.match(AutomataParser.C)
                self.state = 180
                self.match(AutomataParser.T__2)
                self.state = 181
                self.match(AutomataParser.T__6)
                self.state = 182
                self.match(AutomataParser.T__4)
                self.state = 183
                self.ll()
                pass

            elif la_ == 2:
                self.enterOuterAlt(localctx, 2)
                self.state = 184
                self.match(AutomataParser.T__5)
                self.state = 185
                self.match(AutomataParser.T__2)
                self.state = 186
                self.match(AutomataParser.C)
                self.state = 187
                self.match(AutomataParser.T__2)
                self.state = 188
                self.match(AutomataParser.T__4)
                self.state = 189
                self.match(AutomataParser.T__2)
                self.state = 190
                self.match(AutomataParser.C)
                self.state = 191
                self.match(AutomataParser.T__2)
                self.state = 192
                self.match(AutomataParser.T__6)
                pass

            elif la_ == 3:
                self.enterOuterAlt(localctx, 3)

                pass


        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





