
:- module(minimal, [run_simple/4, run_simple_random/4]).

%% Minimal implementations, without support for parameters in rules.

run_simple(Goal, Goal, _, []).
run_simple(InitialState, Goal, [L->R|Rs], [L->R|Out]) :-
  subset(L, InitialState),
  subtract(InitialState, L, IntermState),
  append(IntermState, R, IntermState1),
  run_simple(IntermState1, Goal, Rs, Out).

run_simple_random(_, 0, _, []) :- !.
run_simple_random(State, Depth, Rules, Result) :-
  random_permutation(Rules, Rules1),
  member(L->R, Rules1),
  (subset(L, State) -> (
    subtract(State, L, State1),
    append(State1, R, State2),
    Depth1 is Depth - 1,
    Result = [L->R|Rest],
    run_simple_random(State2, Depth1, Rules, Rest)
  ); (
    Result = []
  )).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(minimal).

test(run_simple) :-
  findall(X, run_simple([b], [house], [[b]->[pig], [pig]->[house]], X), Y),
  Y = [[[b]->[pig], [pig]->[house]]].

:- end_tests(minimal).

:- run_tests.
