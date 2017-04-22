:- module(mateusz_urbanczyk, [resolve/4, prove/2]).

:- op(200, fx, ~).
:- op(500, xfy, v).


resolve(Var, Clause1, Clause2, Resolvent) :-
    variables([Clause1, Clause2], Variables),
    % Omit Source and Number (axiom atom or set of clauses)
    format([Clause1, Clause2], [[InternalClause1, _, _], [InternalClause2, _, _]], Variables),
    merge_(InternalClause1, InternalClause2, Clauses),
    remove_excluded_middle(Var, Clauses, InternalResolvent),
    outputInternalClause(InternalResolvent, Resolvent).


resolveI(Var, InternalClause1, InternalClause2, Resolvent) :-
    InternalClause1 = [Clause1, _, _],
    InternalClause2 = [Clause2, _, _],
    merge_(Clause1, Clause2, MergedClause),
    remove_excluded_middle(Var, MergedClause, ResolventClause),
    Resolvent = [[ResolventClause, _, (Var, InternalClause1, InternalClause2)]].


prove(Clauses, Proof) :-
    variables(Clauses, Variables),
    format(Clauses, InternalClauses, Variables),
    InternalClauses = [FirstClause|TailClauses],
    Passive = [FirstClause],
    Active = TailClauses,
    prove(Active, Passive, BareProof),
    outputProof(BareProof, Proof).

prove([], _, _) :- fail.
% Get first clause and look for proof
prove([ActiveClause|_], Passive, Proof) :-
    resolveAll(ActiveClause, Passive, Resolvents),
    getProof(Resolvents, Proof), !.

% Otherwise, look in tail while appending new resolvents
prove(Active, Passive, Proof) :-
    Active = [ActiveClause|ActiveClauses],
    resolveAll(ActiveClause, Passive, Resolvents),
    append(ActiveClauses, Resolvents, ExtendedActive),
    append([ActiveClause], Passive, ExtendedPassive),
    prove(ExtendedActive, ExtendedPassive, Proof).

% Return all resolvents from clause
resolveAll(_, [], []) :- !.
resolveAll(InternalClause, Passive, Resolvents) :-
    Passive = [PassiveHead|PassiveTail],
    InternalClause = [Contents, _, _],
    flatVariables(Contents, Variables),
    resolveClause(InternalClause, PassiveHead, Variables, ClauseResolvents),
    resolveAll(InternalClause, PassiveTail, ResolventsTail),
    append(ClauseResolvents, ResolventsTail, Resolvents).

%% Get all resolvents from two clauses
resolveClause(_, _, [], []) :- !.
resolveClause(Clause1, Clause2, [Var|VarTail], Resolvents) :-
    Clause1 = [Contents1, _, _],
    Clause2 = [Contents2, _, _],
    member(Var, Contents1),
    member(~Var, Contents2),
    resolveI(Var, Clause1, Clause2, Resolvent),
    resolveClause(Clause1, Clause2, VarTail, ResolventsTail), !,
    append(Resolvent, ResolventsTail, Resolvents).

resolveClause(Clause1, Clause2, [Var|VarTail], Resolvents) :-
    Clause1 = [Contents1, _, _],
    Clause2 = [Contents2, _, _],
    member(Var, Contents1),
    member(~Var, Contents2),
    resolveI(Var, Clause2, Clause1, Resolvent),
    resolveClause(Clause1, Clause2, VarTail, ResolventsTail), !,
    append(Resolvent, ResolventsTail, Resolvents).

resolveClause(Clause1, Clause2, [_|VarTail], Resolvents) :-
    resolveClause(Clause1, Clause2, VarTail, Resolvents).


% Flat operators in Internal Format
flatVariables([], []).
flatVariables([~X|Tail], [X|VariablesTail]) :-
    flatVariables(Tail, VariablesTail), !.

flatVariables([X|Tail], [X|VariablesTail]) :-
    flatVariables(Tail, VariablesTail), !.


%% %% Extract first contradictory clause (which have [] in first place):
%% %% [ [[], Num, Source], ...]
%% %%

getProof([], []) :- fail, !.
getProof([Proof|_], Proof) :-
    [[], _, _] = Proof, !.
getProof([_|Tail], Proof) :-
    getProof(Tail, Proof).

outputProof(Proof, Output) :-
    outputProof(Proof, [], RevOutput),
    reverse(RevOutput, Output).

outputProof(Proof, Acc, Output) :-
    Proof = [Contents, Num, axiom],
    length(Acc, Len),
    Num is Len,
    outputInternalClause(Contents, CommonContents), !,
    append([(CommonContents, axiom)], Acc, Output).

outputProof(Proof, Acc, Output) :-
    Proof = [[], _, (Var, InternalClause1, InternalClause2)],
    outputProof(InternalClause1, Acc, NextAcc),
    outputProof(InternalClause2, NextAcc, SourceOutput),
    InternalClause1 = [_, Num1, _],
    InternalClause2 = [_, Num2, _],
    Num1F is Num1 + 1,
    Num2F is Num2 + 1,
    append([([], (Var, Num1F, Num2F))], SourceOutput, Output).

outputProof(Proof, Acc, Output) :-
    Proof = [Contents, Len, (Var, InternalClause1, InternalClause2)],
    outputProof(InternalClause1, Acc, NextAcc),
    outputProof(InternalClause2, NextAcc, SourceOutput),
    InternalClause1 = [_, Num1, _],
    InternalClause2 = [_, Num2, _],
    length(SourceOutput, Len),
    outputInternalClause(Contents, CommonContents),
    Num1F is Num1 + 1,
    Num2F is Num2 + 1,
    append([(CommonContents, (Var, Num1F, Num2F))], SourceOutput, Output).


%% Get variables from set of clauses
%%
variables([], []).
variables([H|T], S) :-
    clause_vars(H, S1),
    variables(T, S2),
    merge_(S2, S1, S).

% Variables from one clause
clause_vars(~X, [X]) :- !.
clause_vars(X v Y, S) :-
    clause_vars(X, S1),
    clause_vars(Y, S2), !,
    merge_(S1, S2, S).
clause_vars(X, [X]).

%% Merge two lists (sum)
%%
merge_([], X, X).
merge_([H|T], X, [H|S]) :-
    select(H, X, Y), !,
    merge_(T, Y, S).
merge_([H|T], X, [H|S]) :-
    merge_(T, X, S).


%% Convert list to move convenient Internal Format
%% [ [p, ~q, r], [w, q, ~t], ... ]
%%
format([], [], _) :- !.
format([Clause|ClausesTail], [[ConvertedHead, _, axiom]|ConvertedTail], Variables) :-
    create_formatted_list(Clause, [], PreConvertedHead),
    remove_all_excluded_middles(Variables, PreConvertedHead, ConvertedHead),
    format(ClausesTail, ConvertedTail, Variables).


create_formatted_list(X v Y, Input, Result) :-
    create_formatted_list(X, Input, R1),
    create_formatted_list(Y, R1, R2), !,
    merge_(R1, R2, Result).

create_formatted_list(X, Input, [X|Input]) :-
    \+ member(X, Input), !.
create_formatted_list(_, Input, Input).


%% Remove excluded middles
%%
remove_all_excluded_middles([], List, List).
remove_all_excluded_middles([Var|VariablesTail], ConvertedList, Result) :-
    remove_excluded_middle(Var, ConvertedList, PreConvertedList),
    remove_all_excluded_middles(VariablesTail, PreConvertedList, Result).

remove_excluded_middle(Var, List, ReducedList) :-
    member(Var, List),
    member(~Var, List),
    select(Var, List, PreReducedList1),
    select(~Var, PreReducedList1, PreReducedList),
    remove_excluded_middle(Var, PreReducedList, ReducedList), !.
remove_excluded_middle(_, Result, Result).

%% [p, ~q] => p v ~p
outputInternalClause([], []) :- !.
outputInternalClause([A, B], A v B) :- !.
outputInternalClause([A], A) :- !.
outputInternalClause(InternalClause, Output) :-
    InternalClause = [Head|Tail],
    outputInternalClause(Tail, TailOutput), !,
    Output = Head v TailOutput.
