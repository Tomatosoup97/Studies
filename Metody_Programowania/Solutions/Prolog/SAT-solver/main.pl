:- module(main, [solve/2]).

:- op(200, fx, ~).
:- op(500, xfy, v).


solve([], [[]]) :- !.
solve(Clauses, Solution) :-
    variables(Clauses, Variables),
    \+ member([], Variables),
    format(Clauses, InternalFormat1, Variables),
    flattenIfEmpty(InternalFormat1, InternalFormat),    
    sort_internal_list(InternalFormat, SortedInternalList),
    reduce_all_single_var_clauses(SortedInternalList, ProcessedInternalList,
                                  InitialSolution), !,
    unused_variables(Variables, InitialSolution, UnusedVars),
    solve(ProcessedInternalList, UnusedVars, PreSolution),
    append(InitialSolution, PreSolution, NoXSolution),
    set_unsolved_vars(NoXSolution, UnusedVars, Solution).


solve([[X], []], [X], [(X, t)]).
solve([[], [X]], [X], [(X, f)]).
solve([], _, []) :- !.
solve(_, [], _) :- fail.
solve(InternalList, [Var|VarsTail], Solution) :-
    reduce_all_single_var_clauses([ [[Var], []] | InternalList], ReducedList, Solution1),
    unused_variables(VarsTail, Solution1, RemainingVars),
    solve(ReducedList, RemainingVars, Solution2),
    append(Solution1, Solution2, Solution).

solve(InternalList, [Var|RemainingVars], Solution) :-
    reduce_single_var_clause([[], [Var]], InternalList, ReducedList, Solution1),
    solve(ReducedList, RemainingVars, Solution2),
    append(Solution1, Solution2, Solution).


flattenIfEmpty([[[], []]], []).
flattenIfEmpty(List, List).


%% Get unused variables
%% E.g ([p, q, r, s], [(p, t), (q, t)]) => [r, s]
%%
unused_variables([], _, []) :- !.
unused_variables([Var|VarsTail], Solution, UnusedVars) :-
    member((Var, _), Solution), !,
    unused_variables(VarsTail, Solution, UnusedVars).
unused_variables([Var|VarsTail], Solution, [Var|UnusedVarsTail]) :-
    unused_variables(VarsTail, Solution, UnusedVarsTail).


%% Set (Var, x) for each variable which is not in Solution
%%
set_unsolved_vars(Solution, [], Solution):- !.

set_unsolved_vars(Solution, [Var|VariableTail], FullSolution) :-
    \+ member((Var, _), Solution),
    push_front((Var, x), Solution, PreSolution), !,
    set_unsolved_vars(PreSolution, VariableTail, FullSolution).

set_unsolved_vars(Solution, [_|VariableTail], FullSolution) :-
    set_unsolved_vars(Solution, VariableTail, FullSolution).


%% Selection sort by length - shortest first
%%
sort_internal_list([], []).
sort_internal_list(InternalList, [ShortestClause|SortedList]) :-
    select_shortest(InternalList, ShortestClause, Rest),
    sort_internal_list(Rest, SortedList).


select_shortest([Clause], Clause) :- !.
select_shortest([H1, H2|Tail], ShortestClause) :-
    internal_len(H1, Len1),
    internal_len(H2, Len2),
    Len1 =< Len2, !,
    select_shortest([H1|Tail], ShortestClause).

select_shortest([_, H2|Tail], ShortestClause) :-
    select_shortest([H2|Tail], ShortestClause).

select_shortest(InternalList, ShortestClause , Rest) :-
    select_shortest(InternalList, ShortestClause),
    select(ShortestClause, InternalList, Rest).

% Get length of list in Internal Format
internal_len([P, N], Len) :-
    len(P, Len1),
    len(N, Len2),
    Len is Len1 + Len2.

len([], 0).
len([_|Tail], Len) :-
    len(Tail, TailLen),
    Len is TailLen + 1.


%% Finding [[p], []], we can reduce all of the unnecessary values on other
%% clauses using two techniques listed below 
%%
reduce_all_single_var_clauses([], [], []).
reduce_all_single_var_clauses([Head|Tail], ReducedListFinal, [Value|SolutionTail]) :-
    reduce_single_var_clause(Head, Tail, ReducedList, [Value]), !,
    reduce_all_single_var_clauses(ReducedList, ReducedListFinal, SolutionTail).

reduce_all_single_var_clauses([Head|InternalTail], [Head|ReducedTail], Solution) :-
    reduce_all_single_var_clauses(InternalTail, ReducedTail, Solution).


reduce_single_var_clause(InternalElement, InternalList, ReducedListFinal, [(X, t)]) :-
    InternalElement = [[X], []], !, 
    eliminate_solved_clauses((X, t), InternalList, ReducedList),
    remove_negated_values((X, t), ReducedList, ReducedListFinal).

reduce_single_var_clause(InternalElement, InternalList, ReducedListFinal, [(X, f)]) :-
    InternalElement = [[], [X]], !,
    eliminate_solved_clauses((X, f), InternalList, ReducedList),
    remove_negated_values((X, f), ReducedList, ReducedListFinal).
reduce_single_var_clause(List, List, []).


%% If we know that (p, t), we can remove (p v q v r)
%%
eliminate_solved_clauses((_, _), [], []) :- !.
eliminate_solved_clauses((X, t), [Head|Tail], ReducedTail) :-
    Head = [P, _],
    member(X, P), !,
    eliminate_solved_clauses((X, t), Tail, ReducedTail).

eliminate_solved_clauses((_, _), [], []) :- !.
eliminate_solved_clauses((X, f), [Head|Tail], Tail) :-
    Head = [_, N],
    member(X, N), !,
    eliminate_solved_clauses((X, f), Tail, Tail).

eliminate_solved_clauses((X, Flag), [Head|Tail], [Head|ReducedTail]) :-
    eliminate_solved_clauses((X, Flag), Tail, ReducedTail).


%% If we know that (p, t), we can reduce (~p v q v r) => (q v r)
%%
remove_negated_values((_, _), [], []) :- !.
remove_negated_values((X, t), [Head|Tail], ReducedList) :-
    Head = [P, N],
    member(X, N),
    select(X, N, NR),
    ReducedHead = [P, NR], !,
    remove_negated_values((X, t), Tail, ReducedTail),
    ReducedList = [ReducedHead|ReducedTail].

remove_negated_values((X, f), [Head|Tail], ReducedList) :-
    Head = [P, N],
    member(X, P),
    select(X, P, PR),
    ReducedHead = [PR, N], !,
    remove_negated_values((X, f), Tail, ReducedTail),
    ReducedList = [ReducedHead|ReducedTail].

remove_negated_values((X, Flag), [Head|Tail], [Head|ReducedTail]) :-
    remove_negated_values((X, Flag), Tail, ReducedTail).


push_front(Item, List, [Item|List]).


%% Get variables from set of clauses
%%
variables([], []).
variables([H|T], S) :-
    clause_vars(H, S1),
    variables(T, S2),
    merge(S2, S1, S).

% Variables from one clause
clause_vars(~X, [X]) :- !.
clause_vars(X v Y, S) :-
    clause_vars(X, S1),
    clause_vars(Y, S2), !,
    merge(S1, S2, S).
clause_vars(X, [X]).


%% Merge two lists (sum)
%%
merge([], X, X).
merge([H|T], X, [H|S]) :-
    select(H, X, Y), !,
    merge(T, Y, S).
merge([H|T], X, [H|S]) :-
    merge(T, X, S).


%% Convert list to move convenient Internal Format
%% P - Positive literals, N - Negative literals
%% [ [[P], [N]], [[P], [N]], ... ]
%%
format([], [], _) :- !.
format([Clause|ClausesTail], [ConvertedHead|ConvertedTail], Variables) :-
    create_formatted_list(Clause, [[],[]], PreConvertedHead),
    remove_all_excluded_middles(Variables, PreConvertedHead, ConvertedHead),
    format(ClausesTail, ConvertedTail, Variables).


create_formatted_list(~X, Input, Result) :-
    Input = [P1, N1],
    \+ member(X, N1),
    push_front(X, N1, N2),
    Result = [P1, N2], !.

create_formatted_list(X v Y, Input, Result) :-
    create_formatted_list(X, Input, R1),
    create_formatted_list(Y, Input, R2), !,
    R1 = [P1, N1],
    R2 = [P2, N2],
    merge(P1, P2, P3),
    merge(N1, N2, N3),
    Result = [P3, N3].

create_formatted_list(X, Input, Result) :-
    Input = [P1, N1],
    \+ member(X, P1),
    push_front(X, P1, P2),
    Result = [P2, N1], !.


%% Remove excluded middles
%%
remove_all_excluded_middles([], List, List).
remove_all_excluded_middles([Var|VariablesTail], ConvertedList, Result) :-
    remove_excluded_middle(Var, ConvertedList, PreConvertedList),
    remove_all_excluded_middles(VariablesTail, PreConvertedList, Result).

remove_excluded_middle(X, [P, N], [PFinal, NFinal]) :-
    member(X, P),
    member(X, N),
    select(X, P, PR),
    select(X, N, NR),
    remove_excluded_middle(X, [PR, NR], [PFinal, NFinal]), !.
remove_excluded_middle(_, Result, Result).
