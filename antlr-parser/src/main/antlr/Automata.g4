grammar Automata;

@header {package me.fang.parser.antlr;}

start : (
    LEFT_CURLY_BRACKET maybeNewline
        (automataField COMMA maybeNewline)*
        automataField maybeNewline
    RIGHT_CURLY_BRACKET)? EOF;

automataField : STRING COLON automataDef;

automataDef : automata | automataBinaryOperation | automataUnaryOperation;

automataBinaryOperation :
    LEFT_SQUARE_BRACKET maybeNewline
        automataOrString COMMA maybeNewline
        BINARY_OPERATION COMMA maybeNewline
        automataOrString maybeNewline
    RIGHT_SQUARE_BRACKET;

automataUnaryOperation :
    LEFT_SQUARE_BRACKET maybeNewline
        UNARY_OPERATION COMMA maybeNewline
        automataOrString maybeNewline
    RIGHT_SQUARE_BRACKET;

automataOrString : automata | STRING;

automata :
    LEFT_CURLY_BRACKET maybeNewline
        '"alphabet"' COLON listOfStringOrNum COMMA maybeNewline
        '"states"' COLON listOfString COMMA maybeNewline
        '"terminals"' COLON listOfString COMMA maybeNewline
        '"start"' COLON STRING COMMA maybeNewline
        '"sigma"' COLON listOfSigmaObject maybeNewline
    RIGHT_CURLY_BRACKET;

sigmaObject :
    LEFT_CURLY_BRACKET maybeNewline
        '"from"' COLON STRING COMMA maybeNewline
        '"to"' COLON STRING COMMA maybeNewline
        '"by"' COLON STRING_OR_NUM maybeNewline
    RIGHT_CURLY_BRACKET;

listOfString :
    LEFT_SQUARE_BRACKET maybeNewline
        (STRING COMMA maybeNewline)*
        STRING maybeNewline
    RIGHT_SQUARE_BRACKET;

listOfStringOrNum :
    LEFT_SQUARE_BRACKET maybeNewline
        (STRING_OR_NUM COMMA maybeNewline)*
        STRING_OR_NUM maybeNewline
    RIGHT_SQUARE_BRACKET;

listOfSigmaObject :
    LEFT_SQUARE_BRACKET maybeNewline
        (sigmaObject COMMA maybeNewline)*
        sigmaObject maybeNewline
    RIGHT_SQUARE_BRACKET;

maybeNewline : NEWLINE?;

LEFT_CURLY_BRACKET :   '{';
RIGHT_CURLY_BRACKET :  '}';
LEFT_SQUARE_BRACKET :  '[';
RIGHT_SQUARE_BRACKET : ']';
COLON :                ':';
COMMA :                 ',';
BINARY_OPERATION :      '"union"' | '"concat"' | '"diff"' | '"intercect"';
UNARY_OPERATION :       '"star"';
STRING_OR_NUM :         STRING | NUMBER;
STRING :                '"'[a-zA-Z ]+'"';
NUMBER :                [1-9][0-9]*;
WHIESPACE :             (' ' | '\t') -> skip;
NEWLINE :               ('\r'? '\n' | '\r')+;