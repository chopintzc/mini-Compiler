:- consult('parser.pl').

create_empty_table() :-
	empty_assoc(Map),
	b_setval(var, Map).

add_symbol(Key, Value) :-
	b_getval(var, Map),
	get_check(Key, Map, CVal),
	NewValue = [Value|CVal],
	put_assoc(Key, Map, NewValue, NewMap),
	b_setval(var, NewMap).

get_check( Key, Map, Value ) :- get_assoc( Key, Map, Value ).
get_check( _, _, [] ).

get_symbol(Key, Value) :-
	b_getval(var, Map),
	get_assoc(Key, Map, [Value|_]).

remove_symbol(Key) :-
	b_getval(var, Map),
	get_assoc(Key, Map, [_|NewValue]),
	put_assoc(Key, Map, NewValue, NewMap),
	b_setval(var, NewMap).

add_symbol_list([],[]).
add_symbol_list([Key|Keys], [Value|Values]) :-
	add_symbol(Key, Value),
	add_symbol_list(Keys, Values).

print_symbol_list:-
	b_getval(var, Map),
	nl, write(Map).

initialize_functions(Y) :-
	splitfunc(Y).

splitfunc([]):-
	!.
splitfunc([[[X,Y],'(',Z,')','=',W]|T]):-
	Keys = Y,
	Values = [X,Z,W],
	add_symbol(Keys, Values),
	splitfunc(T).

table(Filename) :-
	tokenizer(Filename, File),
	lexer(File, Tokens),
	parse_list(Tokens, StructuredList),
	replace_deeplist(StructuredList, File, NewList),
	create_empty_table(),
	initialize_functions(NewList),
	get_symbol(add,_),
%	nl,write(Value),
	remove_symbol(add),
%	A = [1,1,3], B=[4,5,6],
%	add_symbol_list(A, B),
	print_symbol_list().

test() :-
	create_empty_table(),
	add_symbol(5,1),
	add_symbol(5,3),
	remove_symbol(5),
%	remove_symbol(5),
	print_symbol_list().
