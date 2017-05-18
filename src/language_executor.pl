:- consult('symbol_table.pl').
:- consult('interpreter.pl').

run_program(FileName, Arguments, Result):-
	tokenizer(FileName, File),
	lexer(File, Tokens),
	parse_list(Tokens, StructuredList),
	subst(StructuredList, File, [], NewList),
	create_empty_table(),
	initialize_functions(NewList),
	!, call_function(main, Arguments, Result).

chkpar2([],[],[]).
chkpar2([T|TRes],[[T1,A1]|[ARes]],[A1|A1Res]):-
	((T1=='int',integer(T));
	(T1=='bool',(T==0;T==1))),
	chkpar3(TRes,ARes,A1Res).

chkpar3([],[],[]).
chkpar3([T|TRes],[',',[[T1,A1],ARes]],[A1|A1Res]):-
	((number(T),T1=='int');
	(get_symbol(T, Value),T1=='int',integer(Value))),
	chkpar3(TRes,ARes,A1Res).









