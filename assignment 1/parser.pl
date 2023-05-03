% % input as java code sparated by ;

% s(s(A, B)) --> statement(A), [';'], s(B).

% s(s(A)) --> statement(A).

% statement(statement(A)) --> assignment(A).
% statement(statement(A)) --> conditional(A).
% statement(statement(A)) --> loop(A).

% assignment(assignment(A, B)) --> identifier(A), [=], arithmetic_expression(B).



% conditional(conditional(A, B)) -->  [if], condition(A), statement(B).
% conditional(conditional(A, B, C)) -->[if], condition(A), statement(B), [else], statement(C).

% loop(loop(A, B)) --> [while], condition(A), statement(B).



% condition(condition(A)) --> ['('], relative_expression(A), [')'].

% % relative expression
% relative_expression(relative_expression(A, B)) --> arithmetic_expression(A), ['>'], arithmetic_expression(B).
% relative_expression(relative_expression(A, B)) --> arithmetic_expression(A), ['<'], arithmetic_expression(B).
% relative_expression(relative_expression(A, B)) --> arithmetic_expression(A), ['=='], arithmetic_expression(B).
% relative_expression(relative_expression(A, B)) --> arithmetic_expression(A), ['!='], arithmetic_expression(B).
% relative_expression(relative_expression(A, B)) --> arithmetic_expression(A), ['>='], arithmetic_expression(B).
% relative_expression(relative_expression(A, B)) --> arithmetic_expression(A), ['<='], arithmetic_expression(B).


% % arithmetic expression
% arithmetic_expression(arithmetic_expression(A, B, C)) --> term(A), ['+'], arithmetic_expression(B), {C = '+'}.
% arithmetic_expression(arithmetic_expression(A, B, C)) --> term(A), ['-'], arithmetic_expression(B), {C = '-'}.
% arithmetic_expression(arithmetic_expression(A, B, C)) --> term(A), ['*'], arithmetic_expression(B), {C = '*'}.
% arithmetic_expression(arithmetic_expression(A, B, C)) --> term(A), ['/'], arithmetic_expression(B), {C = '/'}.
% arithmetic_expression(arithmetic_expression(A, B, C)) --> term(A), ['%'], arithmetic_expression(B), {C = '%'}.
% arithmetic_expression(arithmetic_expression(A)) -->  ['('], arithmetic_expression(A), [')'].
% arithmetic_expression(arithmetic_expression(A)) --> term(A).

% % term
% term(term(A)) --> identifier(A).
% term(term(A)) --> number(A).

% % lexicon
% % TOFIX: identifier should not be a number or start with a number
% identifier(identifier(A, B)) --> [H|X], {\+integer(H), atom(X), A = H, B = X}.
% number(number(A)) --> [X], {integer(X), X >= 0, A = X}.


s(S) --> statement(S).
s((S,R)) --> statement(S),s(R).

statement(assignment(A)) --> assignment(A).
statement(conditional(C)) --> conditional(C).
statement(loop(L)) --> loop(L).

assignment((Id,[=],E)) --> identifier(Id),[=], arithmetic_expression(E),[;].
conditional(if(IF)) --> if_statement(IF).
conditional(if_else(C)) --> if_else_statement(C).
loop(while([while],C,S)) --> [while], condition(C), statement(S).

if_statement((C,S)) --> [if], condition(C), statement(S).
if_else_statement((IF,[else],S)) --> if_statement(IF), [else],statement(S).

condition(C) --> ['('], relative_expression(C), [')'].

relative_expression(rel(E1,O,E2)) --> arithmetic_expression(E1), rel_operator(O), arithmetic_expression(E2).


% arithmetic expression
arithmetic_expression(arith(T)) --> term(T).
arithmetic_expression(arith(E)) -->  ['('], arithmetic_expression(E), [')'].
arithmetic_expression(arith(T,O,E)) --> term(T), arith_operator(O), arithmetic_expression(E).

% term
term(term(Id)) --> identifier(Id).
term(term(N)) --> number(N).

% lexicon
identifier(id(FCh,Ch)) --> first_character(FCh), characters(Ch).

first_character(L) --> letter(L).
first_character(Sy) --> symbol(Sy).

% characters([]) --> [].
% characters((FCh,Ch)) --> character(FCh), characters(Ch).

characters([]) --> [].
characters([H|T]) --> [H], {atom_length(H,Len),Len<2,char_type(H, alnum); member(H,[$,'_'])}, characters(T).


character(C) --> letter(C).
character(C) --> symbol(C).
character(C) --> number(C).

% letter(C) --> [C], {atom_chars(C, [Char]), char_type(Char, alpha)}.
letter(C) --> [C], {atom_length(C,Len),Len<2, char_type(C, alpha)}.
% letter --> [C], {member(C, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'])}.
symbol(C) --> [C], {member(C, [$,'_'])}.
number(X) --> [X], {integer(X), X >= 0}.

arith_operator(O) --> [O], {member(O, ['+','-','*','/','%'])}.
% arith_operator --> [*].
% arith_operator --> [/].
% arith_operator --> ['%'].
% arith_operator --> [+].
% arith_operator --> [-].

rel_operator(O) --> [O], {member(O, ['>','<','==','!=','>=','<='])}.
% rel_operator --> [>].
% rel_operator --> [<].
% rel_operator --> [==].
% rel_operator --> ['!='].
% rel_operator --> [>=].
% rel_operator --> [<=].
