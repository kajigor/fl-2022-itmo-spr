grammar MyGrammar;

//-----parser-----
startRule: (item? NEW_LINE)* item? ;

item: ruleStatement
    | token ;

ruleStatement: RULE ':' contents ;

token: TOKEN ':' contents ;

contents: content (VBAR content)* ;

content: expression* ;

expression: term (OPERATOR | '{' NUMBER (',' NUMBER)? '}' )? ;

term: '(' contents ')'
    | '[' contents ']'
    | value ;

value: STRING '..' STRING
     | name
     | STRING
     ;

name: RULE
    | TOKEN ;


//-----lexer-----

COLON: ':' ;
LEFT_BRACE : '{' ;
RIGHT_BRACE : '}' ;
LEFT_PAREN : '(' ;
RIGHT_PAREN : ')' ;
LEFT_BRACKET : '[' ;
RIGHT_BRACKET : ']' ;
COMMA : ',' ;
DOT : '.' ;
DOUBLE_DOT : '..' ;

VBAR: '|' ;
OPERATOR: '+' | '*' | '?' ;
RULE: [a-z] [_a-z0-9]* ;
TOKEN: [A-Z] [_A-Z0-9]* ;
STRING: DOUBLE_QUOTED_STRING | SINGLE_QUOTED_STRING ;
DOUBLE_QUOTED_STRING: '"' ~('"')* '"' ;
SINGLE_QUOTED_STRING: '\'' ~('\'')* '\'' ;
NEW_LINE: ('\r'? '\n')+ ( ' ' | '\t' | '\n' | '\r' | '\f' )*  -> channel(HIDDEN) ;

NUMBER: ('+' | '-')? ('0'..'9')+ ;

WS: ( ' ' | '\t' )+ -> channel(HIDDEN) ;

COMMENT: '//' (~'\n')* -> channel(HIDDEN) ;
