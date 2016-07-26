
:- module(pop_fiction, [random_story/3]).

random_story(State, Rules, Way) :-
  %% setof(R, run_depth(State, 1, Rules, R), Ways),
  %% findnsols(100, R, run_depth1(State, 2000, Rules, R), Ways),
  %% last(Ways, Way),
  run_depth(State, 50, Rules, Way).
  %% , print_term(Way, []).

%% Stops when maximum depth is reached, and when we run out of ways to apply
%% rules. Can be made to backtrack with a soft cut instead.
run_depth(_, 0, _, []) :- !.
run_depth(State, Depth, Rules, Applied) :-
  %% a soft cut will give us many solutions
  (random_transition(State, Rules, L->R, State1) -> (
    Depth1 is Depth - 1,
    writeln(Depth1),
    Applied = [L->R|Rest],
    run_depth(State1, Depth1, Rules, Rest)
  ); (
    Applied = []
  )).

%% Outputs all ways to transition towards the goal.
run_goal(Goal, Goal, _, []).
run_goal(State, Goal, Rules, [Lhs->Rhs|Applied]) :-
  State \= Goal,
  transition(State, Rules, Lhs->Rhs, State1),
  run_goal(State1, Goal, Rules, Applied).

%% Nondeterministically transition to another state. Outputs rule used and
%% eventual state.
transition(State, Rules, L2->R1, State1) :-
  member(L->R, Rules),
  copy_term(L->R, L1->R1),
  permutation(L1, L2),
  find_some_unifier(State, L2->R1, State1).

%% Nondeterministically transition in a random order.
random_transition(State, Rules, L->R, State1) :-
  random_permutation(Rules, Rules1),
  transition(State, Rules1, L->R, State1).

%% Given a state and a rule, nondeterministically applies the rule, unifying
%% in all possible orders. Outputs the resulting fully-instantiated state.
find_some_unifier(InitialState, []->Rhs, R) :-
  append(InitialState, Rhs, R).
find_some_unifier(InitialState, [L|Ls]->Rhs, R) :-
  select(Elt, InitialState, IntermState),
  L = Elt,
  find_some_unifier(IntermState, Ls->Rhs, R).

%% Removes duplicate rule lists. Meant to be used with predicates which return
%% all ways to apply rules. Rules are considered duplicate if they are the same
%% on both sides when sorted.
remove_duplicate_rules(In, X) :-
  maplist(sorted_lhs_rules, In, SortedLhs),
  sort(1, @<, SortedLhs, SortedLhs1),
  pairs_values(SortedLhs1, X).

sorted_lhs(Lhs->Rhs, -(Combined, Lhs->Rhs)) :-
  sort(Lhs, Lhs1),
  sort(Rhs, Rhs1),
  append(Lhs1, Rhs1, Combined).

sorted_lhs_rules(Rules, Keys-Rules) :-
  maplist(sorted_lhs, Rules, Pairs),
  pairs_keys(Pairs, Keys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(pop_fiction).

test(remove_duplicate_rules) :-
  remove_duplicate_rules([[[a, b]->[c, d]], [[b, a]->[d, c]]], R),
  R = [[[a, b]->[c, d]]].

test(find_some_unifier_basic) :-
  setof(A, find_some_unifier([jealous(othello)], [jealous(A)]->a, a), A),
  A = [othello].

requires_lhs_permutation(A, B, C) :-
  find_some_unifier([eros(o, d), eros(d, c)], [eros(B, C), eros(A, B)]->a, a).

test(find_some_unifier) :-
  setof(A, requires_lhs_permutation(A, _, _), X),
  setof(B, requires_lhs_permutation(_, B, _), Y),
  setof(C, requires_lhs_permutation(_, _, C), Z),
  X = [o], Y = [d], Z = [c].

othello(X) :- run_goal(
  [eros(desdemona, cassio), eros(othello, desdemona)],
  [eros(desdemona, cassio), eros(cassio, desdemona),
    anger(othello, desdemona), anger(othello, cassio)],
  [([eros(A, B), eros(Witness, A)] ->
    [eros(A, B), eros(B, A), anger(Witness, A), anger(Witness, B)])], X).

test(run_othello) :-
  setof(X, othello(X), Y),
  Y = [
    [[eros(desdemona, cassio), eros(othello, desdemona)]->
      [eros(desdemona, cassio), eros(cassio, desdemona),
        anger(othello, desdemona), anger(othello, cassio)]],
    [[eros(othello, desdemona), eros(desdemona, cassio)]->
      [eros(desdemona, cassio), eros(cassio, desdemona),
        anger(othello, desdemona), anger(othello, cassio)]]],
  remove_duplicate_rules(Y, Z),
  length(Z, 1).

pig(X) :- run_goal(
  [pig, pig, pig, material(straw), material(brick), material(stick), wolf],
  [house(brick)],
  [
    ([pig, material(A)] -> [house(A)]),
    ([wolf, house(straw)] -> [wolf]),
    ([wolf, house(stick)] -> [wolf]),
    ([wolf, house(brick)] -> [house(brick)])
  ], X).

test(run_pig) :-
  setof(X, pig(X), Y),
  remove_duplicate_rules(Y, Z),
  length(Z, 30).

%% Other search strategies

%% Stops after every transition. This avoids missing solutions, but we have to
%% get 100 solutions in order to get the first solution of length 100.
run_any_depth(_, _, _, []).
run_any_depth(State, Depth, Rules, [L->R|Applied]) :-
  transition(State, Rules, L->R, State1),
  Depth1 is Depth - 1,
  run_any_depth(State1, Depth1, Rules, Applied).

test(run_any_depth) :-
  findnsols(6, X, run_any_depth([a], 3, [[a]->[b], [a]->[a]], X), R), !,
  R = [[], [[a]->[b]], [[a]->[a]], [[a]->[a], [a]->[b]], [[a]->[a], [a]->[a]], [[a]->[a], [a]->[a], [a]->[b]]].
  
%% Stops only if maximum depth is reached. Misses solutions which run out of ways
%% to apply rules, but have not yet reached maximum depth.
run_fixed_depth(_, 0, _, []) :- !. % cut so we don't have to do \= 0
run_fixed_depth(State, Depth, Rules, [L->R|Applied]) :-
  transition(State, Rules, L->R, State1),
  Depth1 is Depth - 1,
  run_fixed_depth(State1, Depth1, Rules, Applied).

test(run_fixed_depth) :-
  findall(X, run_fixed_depth([a],3,[[a]->[b],[a]->[a]], X), R),
  R = [[[a]->[a], [a]->[a], [a]->[b]], [[a]->[a], [a]->[a], [a]->[a]]].

:- end_tests(pop_fiction).
