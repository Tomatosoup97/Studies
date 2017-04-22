% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko}_eval gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(imie_nazwisko_eval, [run/5]).

% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.
run(Program, FName, Arg, Value, Clauses) :-
  Program = [def('main', wildcard(no), num(no, 42))],
  FName = 'main',
  Arg = [a,b,c],
  Value = 42,
  Clauses = [].
