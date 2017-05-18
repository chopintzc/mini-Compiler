
% top-level call:
tokenizer(FileName, TokenList) :-
	read_file(FileName, I),
	insert(32, 46, I, L),
	replace(9,32,L,M),
	replace(10,32,M,N),
	replace(13,32,N,O),
	atom_codes(W,O),
        atomic_list_concat(T,' ',W),
	removeAll(T,'',TokenList),
        printlist(TokenList).

insert(_, _, [], []).
insert(X, Y, [Y|Rest0], [X,Y|Rest1]) :-
    !, insert(X, Y, Rest0, Rest1).
insert(X, Y, [W|Rest0], [W|Rest1]) :-
    insert(X, Y, Rest0, Rest1).

% Read in a file (a character ata  time) and turn this into a list
read_file(FileName, CharList) :-
	open(FileName, read, Stream),
	read_list(Stream, CharList),
	close(Stream).

read_list(Stream, [C|L]) :-
	get0(Stream, C),
	C \== -1, % end of file reached
	!,
	read_list(Stream, L).
read_list(_, []).

% Remove every instance of the specified element from the given list
removeAll( [], _, [] ) :- !.
removeAll( [Element|Tail], Element, NewList ) :-
	!, removeAll( Tail, Element, NewList ).
removeAll( [Head|Tail], Element, [Head|NewList] ) :-
	!, removeAll( Tail, Element, NewList ).

% replace
replace(_, _ , [], []):-
	!.
replace(X, Y, [X|Z], [Y|T]):-
	! ,
	replace(X, Y, Z, T).
replace(X, Y, [W|Z], [W|T]) :-
	replace(X, Y, Z, T).

% printlist
printlist([]).
printlist([X|List]) :-
        write(X),tab(1),
        printlist(List).


