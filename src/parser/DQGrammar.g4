grammar DQGrammar;

// Lexer Rules

PROBE_ID: 's';
BEHAVIOR_TYPE: 'f' | 'a' | 'p';
NUMBER: [0-9]*'.'[0-9]+ | [0-9]+;
IDENTIFIER: [a-zA-Z_][a-zA-Z0-9_]*;
WS: [ \t\r\n]+ -> skip;


// Parser Rules
start: definition* system? EOF;

definition: IDENTIFIER '=' component+   ('->' component)* ';';

system: 'system' '=' component+ ('->' component)*  ';'?;


component
    : behaviorComponent   #behavior
    | probeComponent       #probe
    | outcome                                    #plainOutcome
;

behaviorComponent
    : BEHAVIOR_TYPE ':' IDENTIFIER ('[' probability_list ']')? '(' component_list ')'
;

probeComponent
    : PROBE_ID ':' IDENTIFIER
;


probability_list: NUMBER (',' NUMBER)+;  
component_list: component (',' component)+; 

outcome: IDENTIFIER;
