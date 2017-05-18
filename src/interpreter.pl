call_function( FunctionName, Arguments, Result ) :-
	get_symbol(FunctionName,[FuncType,Parameters,FuncBody]),
	chkpar2(Arguments,Parameters,Args),
	add_symbol_list(Args, Arguments),
	exp(FuncBody,Result).
%	((FuncType=='int',integer(Result));
%	FuncType=='bool',(Result==0;Result==1)).

exp([Value, Extra],Result):-
	value(Value,Val1),
	extraexp(Extra,ExResult),
	eval(Val1,ExResult,Result).
exp([if,Cmp,then,Value1,else,Value2],Result):-
	value(Value1,Val1),
	value(Value2,Val2),
	cmpr(Cmp, CmpResult),
	((CmpResult==1,cmp1(Val1,Val2,CmpResult,Result));
	(CmpResult==0,cmp2(Val1,Val2,CmpResult,Result))).
exp([let,Id,=,[Number1],in,Exp],Result):-
	add_symbol(Id, Value1),
	number(Number1,Value1),
	Exp = [[Func,ValP],[]],
	get_symbol(Func, [ReturnType,Arguments,_]),
	chkpar(ValP,Arguments,P),
	alc(P),
	exp(Exp,Result),
%	chktype(ReturnType,Result),
	dealc(P),
	remove_symbol(Id).

cmpr([Value1, [Opt,Value2]],Result):-
	value(Value1,Val1),
	value(Value2,Val2),
	((Opt=='==', ((Val1==Val2,Result=1));(Val1\=Val2,Result=0));
	(Opt=='!=', ((Val1\=Val2,Result=1));(Val1==Val2,Result=0));
	(Opt=='>', ((Val1>Val2,Result=1));(Val1=<Val2,Result=0));
	(Opt=='>=', ((Val1>=Val2,Result=1));(Val1<Val2,Result=0))).

cmp1(ValL,ValR,CmpResult,Result):-
	((CmpResult==1, Result is ValL);
	(CmpResult==0, Result is ValR)).
cmp2(ValL,ValR,CmpResult,Result):-
	nl,write(ValR),
	Result = ValR.

number(Number,Value):-
	atom_number(Number, Value).

extraexp([],[]).
extraexp([[+,Val]],[+,ValResult]):-
	value(Val,ValResult).

value([Num],Value):-
	Value=Num.
value([Num,[]],Value):-
	get_symbol(Num, Value),
	!.
value([Id,ValP],Value):-
	get_symbol(Id, [ReturnType,Arguments,FuncBody]),
	chkpar(ValP,Arguments,P),
	valp(ValP,Arguments),
	exp(FuncBody,Value),
%	chktype(ReturnType,Value),
	dealc(P).

valp([],[]).
valp(['(',Parameters,')'],Arguments):-
	parameters(Parameters,Arguments).

parameters([[Value],[]],[[T1,P1],[]]):-
	((number(Value,Num),add_symbol(P1,Num));
	(get_symbol(Value,Val),add_symbol(P1,Val))).
parameters([[Value,[]],PList], [[_,P1],Res]):-
	((number(Value,Num),add_symbol(P1,Num));
	(get_symbol(Value,Val),add_symbol(P1,Val))),
	plist(PList,Res).

plist([],[]).
plist([',',PList],[',',Res]):-
	parameters(PList,Res).

eval(Num1,['+',Num2],Result):-
	Result is Num1+Num2.
eval(Num1,['-',Num2],Result):-
	Result is Num1-Num2.
eval(Num,[],Num).

chkpar([],[],[]).
chkpar(['(',[[T,[]]|[TRes]],')'],[[T1,A1]|[ARes]],[A1|A1Res]):-
	get_symbol(T, Value),
	((T1=='int',integer(Value));
	(T1=='float',float(Value));
	(T1=='bool',(Value==0;Value==1))),
	chkpar1(TRes,ARes,A1Res).
chkpar1([],[],[]).
chkpar1([',',[[T],TRes]],[',',[[T1,A1],ARes]],[A1|A1Res]):-
	((number(T,_),T1=='int');
	(get_symbol(T, Value),T1=='int',integer(Value))),
	chkpar1(TRes,ARes,A1Res).
chkpar1([',',[[T,[]],TRes]],[',',[[T1,A1],ARes]],[A1|A1Res]):-
	((number(T,_),T1=='int');
	(get_symbol(T, Value),T1=='int',integer(Value))),
	chkpar1(TRes,ARes,A1Res).

chktype([],[]).
chktype(T1,A1):-
	((T1=='int',integer(A1));
	(T1=='bool',(A1==0;A1==1))).

dealc([]).
dealc([P|PTail]):-
	remove_symbol(P),
	dealc(PTail).

alc([]).
alc([P|PTail]):-
	get_symbol(P,Value),
	add_symbol(P,Value),
	alc(PTail).
