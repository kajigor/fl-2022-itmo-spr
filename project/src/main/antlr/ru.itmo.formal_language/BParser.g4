grammar BParser;

/*
 * Parser Rules
 */

@header {
    package ru.itmo.formal_language;
}

start : e EOF;

e : definitions SEP_MAIN goal //
  | SEP_MAIN goal
  ;

definitions : definition | definition definitions ;
definition : atom SEP_DEF goalExpr '.' | atom '.' ;

atom : identifier | identifier '(' listArg ')';

listArg : arg ',' listArg | arg ;

arg : var | atom | FALSE | TRUE ;

var : WORD_START_UPPER_CASE ;
identifier : WORD_START_LOWER_CASE ;

goal : goalExpr '.' ;

goalExpr : <assoc=right> goalExpr op=CONJ goalExpr
         | <assoc=right> goalExpr op=DISJ goalExpr
         | atom
         ;

/*
 * Lexer Rules
 */
WS : [ \t\r\n]+ -> skip ;

FALSE: 'false' ;
TRUE: 'true' ;
CONJ: '/\\' ;
DISJ: '\\/' ;
SEP_DEF: ':-' ;
SEP_MAIN: '?' ;

WORD_START_UPPER_CASE: UPPERCASE TEXT | UPPERCASE ;
WORD_START_LOWER_CASE: LOWERCASE TEXT | LOWERCASE ;
TEXT: (LOWERCASE | UPPERCASE | NUMBER)+ ;
UPPERCASE: [A-Z] ;
LOWERCASE: [a-z] ;
NUMBER: [0-9] ;


