
:- module(pop_fiction, [random_story/3, random_story_goal/5, render_rules/2, render_rules/3]).

rule(L->R, rule{name: none, rule: L->R, text: []}).

render_rules(Rule, Result) :-
  Template = Rule.text,
  atomic_list_concat(Template, ' ', Result).

render_rules(Causality, Rule, Result) :-
  (_->R) = Rule.rule,

  %% find something with a cause
  %% writeln(Causality),
  %% writeln(a),
  include(has_key(Causality), R, R1),
  %% writeln(b),
  maplist(get_from_assoc(Causality), R1, R2),
  %% writeln(R2),

  (is_list_empty(R2) ->
    Additions = [];
    (random_select(Elt, R2, _),
    Additions = [because|Elt])),

  %% writeln(d),

  Template = Rule.text,
  append(Template, Additions, Template1),
  atomic_list_concat(Template1, ' ', Result).

is_list_empty([]).
%% is_list_empty([_|_], false).

get_from_assoc(A, L, R) :-
  get_assoc(L, A, R).

has_key(A, L) :-
  get_assoc(L, A, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_story(State, Rules, Way) :-
  %% setof(R, run_depth(State, 1, Rules, R), Ways),
  %% findnsols(100, R, run_depth1(State, 2000, Rules, R), Ways),
  %% last(Ways, Way),
  run_depth(State, 20, Rules, Way).
  %% , print_term(Way, []).

random_story_goal(State, Goal, Rules, Causality, Way) :-
  empty_assoc(Causality0),
  run_goal_incl(State, Goal, Rules, Causality0, Causality, Way).

%% Stops when maximum depth is reached, and when we run out of ways to apply
%% rules. Can be made to backtrack with a soft cut instead.
run_depth(_, 0, _, []) :- !.
run_depth(State, Depth, Rules, Applied) :-
  (random_transition(State, Rules, ChosenRule, State1) -> (
    Depth1 is Depth - 1,
    writeln(Depth1),
    Applied = [ChosenRule|Rest],
    run_depth(State1, Depth1, Rules, Rest)
  ); (
    Applied = []
  )).

%% Outputs all ways to transition towards some state which includes the goal.
run_goal_incl(State, Goal, _, Causality, Causality, []) :-
  member(Goal, State).
run_goal_incl(State, Goal, Rules, Causality0, Causality, [ChosenRule|Applied]) :-
  \+ member(Goal, State),
  random_transition(State, Rules, ChosenRule, State1),
  update_causality(ChosenRule, Causality0, Causality1),
  run_goal_incl(State1, Goal, Rules, Causality1, Causality, Applied).

update_causality(Rule, Causality, Causality1) :-
  (L->R) = Rule.rule,
  Rendering = Rule.text,
  % anything introduced on the right side is there because of this rule
  subtract(R, L, New),
  maplist(reverse_pair(Rendering), New, New1),
  foldl(update_assoc, New1, Causality, Causality1).

pair(A, B, A-B).
reverse_pair(B, A, A-B).

update_assoc(K-V, A, A1) :-
  % only update, so causality can't refer to future events. not sure if this works
  (\+ get_assoc(K, A, V)) ->
  put_assoc(K, A, V, A1).

%% Outputs all ways to transition towards the goal state.
run_goal(Goal, Goal, _, []).
run_goal(State, Goal, Rules, [ChosenRule|Applied]) :-
  State \= Goal,
  transition(State, Rules, ChosenRule, State1),
  run_goal(State1, Goal, Rules, Applied).

%% Nondeterministically transition to another state. Outputs rule used and
%% eventual state.
transition(State, Rules, Rule, State1) :-
  member(Rule1, Rules),
  copy_term(Rule1, Rule2),
  (L->R) = Rule2.rule,
  permutation(L, L1),
  Rule = Rule2.put(_{rule: L1->R}),
  find_some_unifier(State, Rule, State1).

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
%% all ways to apply rules. Rules are considered duplicate if both sides are the
%% same when sorted.
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
