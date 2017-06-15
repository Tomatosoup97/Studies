:- module(eval, [run/5]).


run(Program, FName, Arg, Value, Clauses) :-
  Program = [def('main', wildcard(no), num(no, 42))],
  FName = 'main',
  Arg = [a,b,c],
  Value = 42,
  Clauses = [].
