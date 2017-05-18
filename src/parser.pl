:- consult('lexer.pl').

program(Y) --> functionlist(X), {Y = X}.
functionlist(Z) --> functions(X), functionlistcollection(Y), {Z = [X|Y]}.
functionlistcollection(Y) --> functionlist(X), {Y = X}.
functionlistcollection([]) --> [].
functions(W) --> typeid(X), ['OPEN_P'], typeidlist(Y), ['CLOSE_P'], ['ASSIGN'], expression(Z), {W = [X,'(',Y,')','=',Z]}.
typeid(X) --> ['TYPE_INT'], ['IDENTIFIER'], {X = ['int', 'id']}.
typeid(X) --> ['TYPE_BOOL'], ['IDENTIFIER'], {X = ['bool', 'id']}.
typeidlist(Z) --> typeid(X), typeidlistcollection(Y), {Z = [X,Y]}.
typeidlistcollection(Y) --> ['COMMA'], typeidlist(X), {Y = [',', X]}.
typeidlistcollection([]) --> [].
expression(W)  --> ['COND_IF'], comparison(X), ['COND_THEN'], value(Y), ['COND_ELSE'], value(Z), {W = ['if', X, 'then', Y, 'else', Z]}.
expression(Z) --> ['LET'], ['IDENTIFIER'], ['ASSIGN'], value(X), ['LET_IN'], expression(Y), {Z = ['let','id','=',X,'in',Y]}.
expression(Z) --> value(X), extraexpression(Y), {Z = [X,Y]}.
extraexpression(Y) --> arithmetic(X), {Y = [X]}.
extraexpression([]) --> [].
arithmetic(Y) --> ['ARITH_ADD'], value(X), {Y = ['+',X]}.
arithmetic(Y) --> ['ARITH_SUB'], value(X), {Y = ['-',X]}.
comparison(Z) --> value(X), comparisonright(Y), {Z = [X, Y]}.
comparisonright(Y) --> ['LOGIC_EQ'], value(X), {Y = ['==', X]}.
comparisonright(Y) --> ['LOGIC_NOT_EQ'], value(X), {Y = ['!=', X]}.
comparisonright(Y) --> ['LOGIC_GT'], value(X), {Y = ['>', X]}.
comparisonright(Y) --> ['LOGIC_GTEQ'], value(X), {Y = ['>=', X]}.
value(X) --> ['INTEGER'], {X = ['number']}.
value(Y) --> ['IDENTIFIER'], valueparameters(X), {Y = ['id',X]}.
valueparameters(Y) --> ['OPEN_P'], parameters(X), ['CLOSE_P'], {Y = ['(',X,')']}.
valueparameters([]) --> [].
parameters(Z) --> value(X), parameterslist(Y), {Z = [X,Y]}.
parameterslist(Y) --> ['COMMA'], parameters(X), {Y = [',',X]}.
parameterslist([]) --> [].

parse_list(LexedList, StructuredList):- phrase(program(StructuredList), LexedList).

subst([], A, A, []).
subst([Hs|Ts], Tokens, RemainingTokens, [R1|R2]):-
	is_list(Hs),
	subst(Hs, Tokens, TempTok, R1),
	subst(Ts, TempTok, RemainingTokens, R2).
subst([_|Ts], [Ht|Tt], TempTok, [Ht|R1]):-
	subst(Ts, Tt, TempTok, R1).


parsing(Filename, NewList) :-
	tokenizer(Filename, File),
	lexer(File, Tokens),
	parse_list(Tokens, StructuredList),
	replace_deeplist(StructuredList, File, NewList).

