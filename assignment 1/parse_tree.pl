s(Tree) --> statement(Tree).
s(tree(statement(Tree1), Tree2)) --> statement(Tree1), s(Tree2).

statement(Tree) --> assignment(Tree).
statement(Tree) --> conditional(Tree).
statement(Tree) --> loop(Tree).

assignment(tree(identifier(Identifier), '=', arithmetic_expression(Tree))) --> identifier(Identifier), ['='], arithmetic_expression(Tree), [;].

conditional(Tree) --> if_statement(Tree).
conditional(Tree) --> if_else_statement(Tree).

if_statement(tree(if(condition(Tree1), statement(Tree2)))) --> [if], condition(Tree1), statement(Tree2).

if_else_statement(tree(if(condition(Tree1), statement(Tree2)), else(statement(Tree3)))) --> if_statement(tree(if(condition(Tree1), statement(Tree2)))), [else], statement(Tree3).

loop(tree(while(condition(Tree1), statement(Tree2)))) --> [while], condition(Tree1), statement(Tree2).

condition(Tree) --> ['('], relative_expression(Tree), [')'].

relative_expression(tree(arithmetic_expression(Tree1), rel_operator(Rel), arithmetic_expression(Tree2))) --> arithmetic_expression(Tree1), rel_operator(Rel), arithmetic_expression(Tree2).

% arithmetic expression
arithmetic_expression(Tree) --> term(Tree).
arithmetic_expression(tree(arithmetic_expression(Tree1), arith_operator(Op), term(Tree2))) --> term(Tree1), arith_operator(Op), arithmetic_expression(Tree2).
arithmetic_expression(Tree) --> ['('], arithmetic_expression(Tree), [')'].

% term
term(tree(identifier(Identifier))) --> identifier(Identifier).
term(tree(number(Number))) --> number(Number).

% lexicon
identifier(tree(first_character(Letter), characters(Tail))) --> first_character(Letter), characters(Tail).

first_character(tree(letter(Char))) --> letter(Char).
first_character(tree(symbol(Sym))) --> symbol(Sym).

characters(tree(character(Char), characters(Tail))) --> character(Char), characters(Tail).
characters(tree()) --> [].

character(tree(letter(Char))) --> letter(Char).
character(tree(symbol(Sym))) --> symbol(Sym).
character(tree(number(Number))) --> number(Number).

letter(Char) --> [C], {atom_chars(C, [Char]), char_type(Char, alpha)}.
letter(Char) --> [C], {atom_length(C,Len),Len<2, char_type(C, alpha)}, {atom_chars(C, [Char])}.
symbol(Sym) --> [C], {member(C, [$,'_'])}, {atom_chars(Sym, [C])}.
number(tree(number(Number))) --> [X], {integer(X), X >= 0}, {Number = X}.

arith_operator(tree(Op)) --> [Op], {member(Op, ['+', '-', '*', '/', '%'])}.
rel_operator(tree(Op)) --> [Op], {member(Op, ['<', '>', '==', '!=', '>=', '<='])}.