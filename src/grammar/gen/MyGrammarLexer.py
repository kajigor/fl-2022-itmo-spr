# Generated from /home/kate/PycharmProjects/fl-2022-itmo-spr/src/grammar/MyGrammar.g4 by ANTLR 4.10.1
from antlr4 import *
from io import StringIO
import sys

from antlr4.error.Errors import LexerNoViableAltException

if sys.version_info[1] > 5:
    from typing import TextIO
else:
    from typing.io import TextIO


def serializedATN():
    return [
        4,0,19,138,6,-1,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,
        2,6,7,6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,
        13,7,13,2,14,7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,1,0,1,
        0,1,1,1,1,1,2,1,2,1,3,1,3,1,4,1,4,1,5,1,5,1,6,1,6,1,7,1,7,1,7,1,
        8,1,8,1,9,1,9,1,10,1,10,5,10,63,8,10,10,10,12,10,66,9,10,1,11,1,
        11,5,11,70,8,11,10,11,12,11,73,9,11,1,12,1,12,3,12,77,8,12,1,13,
        1,13,5,13,81,8,13,10,13,12,13,84,9,13,1,13,1,13,1,14,1,14,5,14,90,
        8,14,10,14,12,14,93,9,14,1,14,1,14,1,15,3,15,98,8,15,1,15,4,15,101,
        8,15,11,15,12,15,102,1,15,5,15,106,8,15,10,15,12,15,109,9,15,1,15,
        1,15,1,16,3,16,114,8,16,1,16,4,16,117,8,16,11,16,12,16,118,1,17,
        4,17,122,8,17,11,17,12,17,123,1,17,1,17,1,18,1,18,1,18,1,18,5,18,
        132,8,18,10,18,12,18,135,9,18,1,18,1,18,0,0,19,1,1,3,2,5,3,7,4,9,
        5,11,6,13,7,15,8,17,9,19,10,21,11,23,12,25,13,27,14,29,15,31,16,
        33,17,35,18,37,19,1,0,11,2,0,42,43,63,63,1,0,97,122,3,0,48,57,95,
        95,97,122,1,0,65,90,3,0,48,57,65,90,95,95,1,0,34,34,1,0,39,39,3,
        0,9,10,12,13,32,32,2,0,43,43,45,45,2,0,9,9,32,32,1,0,10,10,149,0,
        1,1,0,0,0,0,3,1,0,0,0,0,5,1,0,0,0,0,7,1,0,0,0,0,9,1,0,0,0,0,11,1,
        0,0,0,0,13,1,0,0,0,0,15,1,0,0,0,0,17,1,0,0,0,0,19,1,0,0,0,0,21,1,
        0,0,0,0,23,1,0,0,0,0,25,1,0,0,0,0,27,1,0,0,0,0,29,1,0,0,0,0,31,1,
        0,0,0,0,33,1,0,0,0,0,35,1,0,0,0,0,37,1,0,0,0,1,39,1,0,0,0,3,41,1,
        0,0,0,5,43,1,0,0,0,7,45,1,0,0,0,9,47,1,0,0,0,11,49,1,0,0,0,13,51,
        1,0,0,0,15,53,1,0,0,0,17,56,1,0,0,0,19,58,1,0,0,0,21,60,1,0,0,0,
        23,67,1,0,0,0,25,76,1,0,0,0,27,78,1,0,0,0,29,87,1,0,0,0,31,100,1,
        0,0,0,33,113,1,0,0,0,35,121,1,0,0,0,37,127,1,0,0,0,39,40,5,58,0,
        0,40,2,1,0,0,0,41,42,5,123,0,0,42,4,1,0,0,0,43,44,5,125,0,0,44,6,
        1,0,0,0,45,46,5,40,0,0,46,8,1,0,0,0,47,48,5,41,0,0,48,10,1,0,0,0,
        49,50,5,44,0,0,50,12,1,0,0,0,51,52,5,46,0,0,52,14,1,0,0,0,53,54,
        5,46,0,0,54,55,5,46,0,0,55,16,1,0,0,0,56,57,5,124,0,0,57,18,1,0,
        0,0,58,59,7,0,0,0,59,20,1,0,0,0,60,64,7,1,0,0,61,63,7,2,0,0,62,61,
        1,0,0,0,63,66,1,0,0,0,64,62,1,0,0,0,64,65,1,0,0,0,65,22,1,0,0,0,
        66,64,1,0,0,0,67,71,7,3,0,0,68,70,7,4,0,0,69,68,1,0,0,0,70,73,1,
        0,0,0,71,69,1,0,0,0,71,72,1,0,0,0,72,24,1,0,0,0,73,71,1,0,0,0,74,
        77,3,27,13,0,75,77,3,29,14,0,76,74,1,0,0,0,76,75,1,0,0,0,77,26,1,
        0,0,0,78,82,5,34,0,0,79,81,8,5,0,0,80,79,1,0,0,0,81,84,1,0,0,0,82,
        80,1,0,0,0,82,83,1,0,0,0,83,85,1,0,0,0,84,82,1,0,0,0,85,86,5,34,
        0,0,86,28,1,0,0,0,87,91,5,39,0,0,88,90,8,6,0,0,89,88,1,0,0,0,90,
        93,1,0,0,0,91,89,1,0,0,0,91,92,1,0,0,0,92,94,1,0,0,0,93,91,1,0,0,
        0,94,95,5,39,0,0,95,30,1,0,0,0,96,98,5,13,0,0,97,96,1,0,0,0,97,98,
        1,0,0,0,98,99,1,0,0,0,99,101,5,10,0,0,100,97,1,0,0,0,101,102,1,0,
        0,0,102,100,1,0,0,0,102,103,1,0,0,0,103,107,1,0,0,0,104,106,7,7,
        0,0,105,104,1,0,0,0,106,109,1,0,0,0,107,105,1,0,0,0,107,108,1,0,
        0,0,108,110,1,0,0,0,109,107,1,0,0,0,110,111,6,15,0,0,111,32,1,0,
        0,0,112,114,7,8,0,0,113,112,1,0,0,0,113,114,1,0,0,0,114,116,1,0,
        0,0,115,117,2,48,57,0,116,115,1,0,0,0,117,118,1,0,0,0,118,116,1,
        0,0,0,118,119,1,0,0,0,119,34,1,0,0,0,120,122,7,9,0,0,121,120,1,0,
        0,0,122,123,1,0,0,0,123,121,1,0,0,0,123,124,1,0,0,0,124,125,1,0,
        0,0,125,126,6,17,0,0,126,36,1,0,0,0,127,128,5,47,0,0,128,129,5,47,
        0,0,129,133,1,0,0,0,130,132,8,10,0,0,131,130,1,0,0,0,132,135,1,0,
        0,0,133,131,1,0,0,0,133,134,1,0,0,0,134,136,1,0,0,0,135,133,1,0,
        0,0,136,137,6,18,0,0,137,38,1,0,0,0,13,0,64,71,76,82,91,97,102,107,
        113,118,123,133,1,0,1,0
    ]

class MyGrammarLexer(Lexer):

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    COLON = 1
    LEFT_BRACE = 2
    RIGHT_BRACE = 3
    LEFT_PAREN = 4
    RIGHT_PAREN = 5
    COMMA = 6
    DOT = 7
    DOUBLE_DOT = 8
    VBAR = 9
    OPERATOR = 10
    RULE = 11
    TOKEN = 12
    STRING = 13
    DOUBLE_QUOTED_STRING = 14
    SINGLE_QUOTED_STRING = 15
    NEW_LINE = 16
    NUMBER = 17
    WS = 18
    COMMENT = 19

    channelNames = [ u"DEFAULT_TOKEN_CHANNEL", u"HIDDEN" ]

    modeNames = [ "DEFAULT_MODE" ]

    literalNames = [ "<INVALID>",
            "':'", "'{'", "'}'", "'('", "')'", "','", "'.'", "'..'", "'|'" ]

    symbolicNames = [ "<INVALID>",
            "COLON", "LEFT_BRACE", "RIGHT_BRACE", "LEFT_PAREN", "RIGHT_PAREN", 
            "COMMA", "DOT", "DOUBLE_DOT", "VBAR", "OPERATOR", "RULE", "TOKEN", 
            "STRING", "DOUBLE_QUOTED_STRING", "SINGLE_QUOTED_STRING", "NEW_LINE", 
            "NUMBER", "WS", "COMMENT" ]

    ruleNames = [ "COLON", "LEFT_BRACE", "RIGHT_BRACE", "LEFT_PAREN", "RIGHT_PAREN", 
                  "COMMA", "DOT", "DOUBLE_DOT", "VBAR", "OPERATOR", "RULE", 
                  "TOKEN", "STRING", "DOUBLE_QUOTED_STRING", "SINGLE_QUOTED_STRING", 
                  "NEW_LINE", "NUMBER", "WS", "COMMENT" ]

    grammarFileName = "MyGrammar.g4"

    def __init__(self, input=None, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.10.1")
        self._interp = LexerATNSimulator(self, self.atn, self.decisionsToDFA, PredictionContextCache())
        self._actions = None
        self._predicates = None
        self.exception_occurred = None

    # Return a token from self source; i.e., match a token on the char
    #  stream.
    def nextToken(self):
        if self._input is None:
            raise IllegalStateException("nextToken requires a non-null input stream.")

        # Mark start location in char stream so unbuffered streams are
        # guaranteed at least have text of current token
        tokenStartMarker = self._input.mark()
        try:
            while True:
                if self._hitEOF:
                    self.emitEOF()
                    return self._token
                self._token = None
                self._channel = Token.DEFAULT_CHANNEL
                self._tokenStartCharIndex = self._input.index
                self._tokenStartColumn = self._interp.column
                self._tokenStartLine = self._interp.line
                self._text = None
                continueOuter = False
                while True:
                    self._type = Token.INVALID_TYPE
                    ttype = self.SKIP
                    try:
                        ttype = self._interp.match(self._input, self._mode)
                    except LexerNoViableAltException as e:
                        self.notifyListeners(e)  # report error
                        self.recover(e)
                        self.exception_occurred = e
                    if self._input.LA(1) == Token.EOF:
                        self._hitEOF = True
                    if self._type == Token.INVALID_TYPE:
                        self._type = ttype
                    if self._type == self.SKIP:
                        continueOuter = True
                        break
                    if self._type != self.MORE:
                        break
                if continueOuter:
                    continue
                if self._token is None:
                    self.emit()
                return self._token
        finally:
            # make sure we release marker after match or
            # unbuffered char stream will keep buffering
            self._input.release(tokenStartMarker)





