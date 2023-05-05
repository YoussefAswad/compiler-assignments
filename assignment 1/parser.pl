s(s(S)) --> statement(S).
s(s(S,R)) --> statement(S),s(R).

statement(A) --> assignment(A).
statement(C) --> conditional(C).
statement(L) --> loop(L).

assignment(assign(Id,E)) --> identifier(Id),[=], arithmetic_expression(E),[;].
conditional(if(IF)) --> if_statement(IF).
conditional((C)) --> if_else_statement(C).
loop(while([while],C,S)) --> [while], condition(C), statement(S).

if_statement((C,S)) --> [if], condition(C), statement(S).
if_else_statement(if_else(IF,[else],S)) --> if_statement(IF), [else],statement(S).

condition(cond(C)) --> ['('], relative_expression(C), [')'].

relative_expression((E1,O,E2)) --> arithmetic_expression(E1), rel_operator(O), arithmetic_expression(E2).

arithmetic_expression(arith(T,E)) --> term(T), arithmetic_expression2(E).
arithmetic_expression2(X) --> [+],term(T), arithmetic_expression2(E),{X=add(T,E)} | [-], term(T), arithmetic_expression2(E),{X=sub(T,E)}|[],{X=[]}.
term((F,T)) --> factor(F), term2(T).
term2(X) --> [*],factor(F), term2(T),{X=mul(F,T)} | [/], factor(F), term2(T),{X=div(F,T)} | ['%'], factor(F), term2(T),{X=mod(F,T)}| [],{X=[]}.
factor(X) --> ['('], arithmetic_expression(X), [')'].
factor(factor(X)) --> identifier(X) | number(X).

% lexicon
identifier(id([H|T])) --> first_character(H), characters(T).

first_character(L) --> letter(L).
first_character(Sy) --> symbol(Sy).

characters([]) --> [].
characters([H|T]) --> character(H), characters(T).

character(C) --> letter(C).
character(C) --> symbol(C).
character(C) --> number(C).

letter(C) --> [C], {atom_length(C,Len),Len<2, char_type(C, alpha)}.
symbol(C) --> [C], {member(C, [$,'_'])}.
number(num(X)) --> [X], {integer(X), X >= 0}.

rel_operator(O) --> [O], {member(O, ['>','<','==','!=','>=','<='])}.
