
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
  (random_transition(State, Rules, ChosenRule, State1) -> (
    Depth1 is Depth - 1,
    writeln(Depth1),
    Applied = [ChosenRule|Rest],
    run_depth(State1, Depth1, Rules, Rest)
  ); (
    Applied = []
  )).

%% Outputs all ways to transition towards the goal.
run_goal(Goal, Goal, _, []).
run_goal(State, Goal, Rules, [ChosenRule|Applied]) :-
  State \= Goal,
  transition(State, Rules, ChosenRule, State1),
  run_goal(State1, Goal, Rules, Applied).

%% Nondeterministically transition to another state. Outputs rule used and
%% eventual state.
transition(State, Rules, Rule, State1) :-
  %% ground(Rules),
  member(Rule1, Rules),
  copy_term(Rule1, Rule2),
  (L->R) = Rule2.rule,
  permutation(L, L1),
  Rule = Rule2.put(_{rule: L1->R}),
  find_some_unifier(State, Rule, State1).

%% Nondeterministically transition in a random order.
random_transition(State, Rules, ChosenRule, State1) :-
  random_permutation(Rules, Rules1),
  transition(State, Rules1, ChosenRule, State1).

%% Given a state and a rule, nondeterministically applies the rule, unifying
%% in all possible orders. Outputs the resulting fully-instantiated state.
find_some_unifier(State, Rule, Result) :-
  ([]->Rhs) = Rule.rule,
  append(State, Rhs, Result).
find_some_unifier(State, Rule, Result) :-
  ([L|Ls]->Rhs) = Rule.rule,
  select(Elt, State, State1),
  L = Elt,
  Rule1 = Rule.put(_{rule: Ls->Rhs}),
  find_some_unifier(State1, Rule1, Result).

%% Removes duplicate rule lists. Meant to be used with predicates which return
%% all ways to apply rules. Rules are considered duplicate if they are the same
%% on both sides when sorted.
remove_duplicate_rules(In, X) :-
  maplist(sorted_lhs_rules, In, SortedLhs),
  sort(1, @<, SortedLhs, SortedLhs1),
  pairs_values(SortedLhs1, X).

sorted_lhs(Rule, -(Combined, Rule)) :-
  (Lhs->Rhs) = Rule.rule,
  sort(Lhs, Lhs1),
  sort(Rhs, Rhs1),
  append(Lhs1, Rhs1, Combined).

sorted_lhs_rules(Rules, Keys-Rules) :-
  maplist(sorted_lhs, Rules, Pairs),
  pairs_keys(Pairs, Keys).

rule(L->R, rule{name: none, rule: L->R, text: []}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(pop_fiction).

test(remove_duplicate_rules) :-
  rule([a, b]->[c, d], R1),
  rule([b, a]->[d, c], R2),
  remove_duplicate_rules([[R1], [R2]], R),
  R = [[R1]].

test(find_some_unifier_basic) :-
  rule([jealous(A)]->a, R),
  setof(A, find_some_unifier([jealous(othello)], R, a), A),
  A = [othello].

requires_lhs_permutation(A, B, C) :-
  rule([eros(B, C), eros(A, B)]->[a], R),
  find_some_unifier([eros(o, d), eros(d, c)], R, [a]).

test(find_some_unifier) :-
  setof(A, requires_lhs_permutation(A, _, _), X),
  setof(B, requires_lhs_permutation(_, B, _), Y),
  setof(C, requires_lhs_permutation(_, _, C), Z),
  X = [o], Y = [d], Z = [c].

othello(X) :-
  rule([eros(A, B), eros(Witness, A)] ->
    [eros(A, B), eros(B, A), anger(Witness, A), anger(Witness, B)], R),
  run_goal(
    [eros(desdemona, cassio), eros(othello, desdemona)],
    [eros(desdemona, cassio), eros(cassio, desdemona),
      anger(othello, desdemona), anger(othello, cassio)],
    [R], X).

test(run_othello) :-
  setof(X, othello(X), Y),
  rule([eros(desdemona, cassio), eros(othello, desdemona)]->
      [eros(desdemona, cassio), eros(cassio, desdemona),
        anger(othello, desdemona), anger(othello, cassio)], R1),
  rule([eros(othello, desdemona), eros(desdemona, cassio)]->
      [eros(desdemona, cassio), eros(cassio, desdemona),
        anger(othello, desdemona), anger(othello, cassio)], R2),
  Y = [[R1], [R2]],
  remove_duplicate_rules(Y, Z),
  length(Z, 1).

pig(X) :-
  rule([pig, material(A)] -> [house(A)], R1),
  rule([wolf, house(straw)] -> [wolf], R2),
  rule([wolf, house(stick)] -> [wolf], R3),
  rule([wolf, house(brick)] -> [house(brick)], R4),
  run_goal([pig, pig, pig, material(straw), material(brick), material(stick), wolf],
    [house(brick)],
    [R1, R2, R3, R4], X).

test(run_pig) :-
  setof(X, pig(X), Y),
  remove_duplicate_rules(Y, Z),
  length(Z, 30).

:- end_tests(pop_fiction).
