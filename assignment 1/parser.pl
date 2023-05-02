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


s --> statement.
s --> statement,s.

statement --> assignment.
statement --> conditional.
statement --> loop.

assignment --> identifier,[=], arithmetic_expression,[;].
conditional --> if_statement.
conditional --> if_else_statement.
loop --> [while], condition, statement.

if_statement --> [if], condition, statement.
if_else_statement --> if_statement, [else],statement.

condition --> ['('], relative_expression, [')'].

relative_expression --> arithmetic_expression, rel_operator, arithmetic_expression.


% arithmetic expression
arithmetic_expression --> term.
arithmetic_expression -->  ['('], arithmetic_expression, [')'].
arithmetic_expression --> term, arith_operator, arithmetic_expression.

% term
term --> identifier.
term --> number.

% lexicon
identifier --> first_character, characters.

first_character --> letter.
first_character --> symbol.

characters --> [].
characters --> character, characters.

character --> letter.
character --> symbol.
character --> number.

letter --> [C], {atom_chars(C, [Char]), char_type(Char, alpha)}.
letter --> [C], {atom_length(C,Len),Len<2, char_type(C, alpha)}.
% letter --> [C], {member(C, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'])}.
symbol --> [C], {member(C, [$,'_'])}.
number --> [X], {integer(X), X >= 0}.

arith_operator --> [*].
arith_operator --> [/].
arith_operator --> ['%'].

arith_operator --> [+].
arith_operator --> [-].

rel_operator --> [>].
rel_operator --> [<].
rel_operator --> [==].
rel_operator --> ['!='].
rel_operator --> [>=].
rel_operator --> [<=].
