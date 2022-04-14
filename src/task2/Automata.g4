grammar Automata;

s:      '{' lp '}' ;

lp:     '"' C '"' ':' '{' a '}' ',' lp
    |   '"' C '"' ':' '{' a '}'
    |   '"' C '"' ':' '[' lao ']' ',' lp
    |   '"' C '"' ':' '[' lao ']'
    ;

a:      '"glossary"' ':' '[' lc ']' ','
        '"states"' ':' '[' lc ']' ','
        '"initial_state"' ':' '"' C '"' ','
        '"terminal_states"' ':' '[' lc ']' ','
        '"is_dfa"' ':' B ','
        '"edges"' ':' '{' eo '}' ','
        '"edges_epsilon"' ':' '{' ee '}' ;

lao:    '{' a '}' ',' O ',' lao
    |   '"' C '"' ',' O ',' lao
    |   '{' a '}'
    |   '"' C '"'
    ;

lc:     '"' C '"' ',' lc
    |   '"' C '"'
    |
    ;

ee:     '"' C '"' ':' '[' lc ']' ',' ee
    |   '"' C '"' ':' '[' lc ']'
    ;

eo:     '"' C '"' ':' '[' ll ']' ',' eo
    |   '"' C '"' ':' '[' ll ']'
    ;

ll:     '[' '"' C '"' ',' '"' C '"' ']' ',' ll
    |   '[' '"' C '"' ',' '"' C '"' ']'
    |
    ;

O       : '"union"' | '"diff"' | '"intersect"' | '"concat"' | '"star"';
B       : 'true' | 'false';
C       : [a-zA-Z0-9]+;
WS      : [\t\r\n ]+ -> skip;
