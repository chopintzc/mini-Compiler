:- consult('tokenizer.pl').

lexer([],[]).
lexer([H|N],[F|S]):-
	match(H,F),
	lexer(N,S).

match(L,T):-
	L='int', !, T = 'TYPE_INT'.
match(L,T):-
	L='bool', !, T = 'TYPE_BOOL'.
match(L,T):-
	L=',', !, T = 'COMMA'.
match(L,T):-
	L='=', !, T = 'ASSIGN'.
match(L,T):-
	L='in', !, T = 'LET_IN'.
match(L,T):-
	L='let', !, T = 'LET'.
match(L,T):-
	L='if', !, T = 'COND_IF'.
match(L,T):-
	L='then', !, T = 'COND_THEN'.
match(L,T):-
	L='else', !, T = 'COND_ELSE'.
match(L,T):-
	L='==', !, T = 'LOGIC_EQ'.
match(L,T):-
	L='!=', !, T = 'LOGIC_NOT_EQ'.
match(L,T):-
	L='>', !, T = 'LOGI_GT'.
match(L,T):-
	L='>=', !, T = 'LOGIC_GTEQ'.
match(L,T):-
	L='+', !, T = 'ARITH_ADD'.
match(L,T):-
	L='-', !, T = 'ARITH_SUB'.
match(L,T):-
	L='(', !, T = 'OPEN_P'.
match(L,T):-
	L=')', !, T = 'CLOSE_P'.
match(L,T):-
	name(L,[B|Tail]), char_type(B,digit),
	!, match_num(Tail), T = 'INTEGER'.
match(_,T):-
	!, T = 'IDENTIFIER'.

match_num([]).
match_num([A|Res]):-
	char_type(A,digit), match_num(Res).




