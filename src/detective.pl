
:- use_module(pop_fiction).
:- use_module(graph).

main :-
  run,
  halt.

run :-
  state(Initial, Rules),
  generate(Initial, Rules, Way, Graph),
  range(Way, R),
  maplist(get_cause(Way, Graph), R, Causes),
  maplist(render_causal, Causes, Res),

  %% people(Initial, People),
  %% writeln(People),
  %% last(Way, KillingRule),
  %% killed(KillingRule, Dead),
  %% writeln(Dead),
  %% subtract(People, [Dead], StillAlive),
  %% person_mode(StillAlive),

  print_term(Res, []).

person_mode(StillAlive) :-
  length(StillAlive, L),
  range(0, L, X),
  maplist(pair, X, StillAlive, Pairs),
  foreach(member(N-P, Pairs), (write(N), write('. '), writeln(P))),
  writeln('Who do you want to talk to?'),
  (getn(S) -> (
    Y is S + 1,
    writeln(Y),
    writeln(S)
  ); (
    writeln('Please enter a number'),
    person_mode(StillAlive)
  )).
  %% todo have a go back item

talk_mode(Person) :-
  %% list topics
  %% pick a topic
  %% go back
  writeln('talk mode').

topic_mode(Person) :-
  %% list facts
  %% input a number to press on that fact. this can update the list of facts
  %% when max number of presses reached, remove all entries. only have an option to go back
  writeln('topic mode').

getn(S) :-
  read_string(user_input, '\n', '\r', _, S0),
  %% TODO catch errors here and turn this into failure
  string_to_num(S0, S).

gets(S) :-
  read_string(user_input, '\n', '\r', _, S).

string_to_num(S, N) :-
  number_codes(N, S).

people(Initial, People) :-
  findall(X, member(person(X), Initial), People).

killed(Rule, Dead) :-
  (_->R) = Rule.rule,
  once(member(dead(Dead), R)).

render_causal(Rule-Cause, Res) :-
  render_rule(Rule, Rule1),
  render_rule(Cause, Cause1),
  atomic_list_concat([Rule1, Cause1], ' because ', Res).

get_cause(Rules, G, V0, V-Cause) :-
  nth0(V0, Rules, V),
  edges(V0, G, E),
  ([C] = E -> nth0(C, Rules, Cause); Cause =
    rule{
      name: no_reason,
      rule: [] -> [],
      text: ['of no known reason']
    }).

state(Initial, Rules) :-
  Initial = [
    person(agatha),
    person(ben),
    person(charles),
    has(charles, flowers)
  ],
  Rules = [
    rule{
      name: like,
      rule: [person(A), person(B)] ->
        [person(A), person(B), like(A, B)],
      text: [A, 'began to like', B]
    },
    rule{
      name: hate,
      rule: [person(A), person(B)] ->
        [person(A), person(B), hate(A, B)],
      text: [A, 'began to hate', B]
    },
    rule{
      name: steal,
      rule: [person(A), person(B), has(B, I), hate(A, B)] ->
        [person(A), person(B), has(A, I), hate(A, B), hate(B, A)],
      text: [A, 'stole', I, 'from', B]
    },
    rule{
      name: give,
      rule: [person(A), person(B), has(A, I), like(A, B)] ->
        [person(A), person(B), has(B, I), like(A, B), like(B, A)],
      text: [A, 'gave', I, 'to', B]
    },
    rule{
      name: kill,
      rule: [person(A), person(B), hate(A, B), hate(A, B)] ->
        [person(A), dead(B)],
      text: [A, 'killed', B, 'out of spite']
    },
    rule{
      name: jealousy,
      rule: [person(A), person(B), person(C), like(A, B), like(A, B)] ->
        [person(A), person(B), person(C), like(A, B), like(A, B), hate(C, B), hate(C, B)],
      text: [C, 'became jealous of', A, 'and', B]
    }
  ].

generate(Initial, Rules, Way, Graph) :-
  random_story_goal(Initial, dead(_), Rules, Way),

  range(Way, Ix),
  maplist(index, Ix, Way, IxWay),

  all_before(IxWay, AllBefore),
  maplist(pair, IxWay, AllBefore, Pairs),
  graph:graph(Graph0),
  foldl(most_probable_cause_graph, Pairs, Graph0, Graph).

index(N, X, i{i: N, item: X}).

range(Xs, Ys) :-
  length(Xs, L),
  range(0, L, Ys).

range(End, End, []) :- !.
range(Start, End, [Start|Res]) :-
  X is Start + 1,
  range(X, End, Res).

all_before([], []).
all_before([_], [[]]) :- !.
all_before([X|Xs], [[]|Ys]) :-
  all_before(Xs, Zs),
  maplist(cons(X), Zs, Ys).

cons(X, Xs, [X|Xs]).
fst(A-_, A).
snd(_-B, B).
pair(A, B, A-B).

rule_intersection(Rule, Cause, Cause-Size) :-
  (L->_) = Rule.rule,
  (_->Cr) = Cause.rule,
  intersection(L, Cr, R),
  length(R, Size).

indexed_rule_intersection(Rule, Cause, Cause-Size) :-
  R = Rule.item,
  C = Cause.item,
  rule_intersection(R, C, _-Size).

% Finds the rule whose RHS with the largest intersection with the LHS of this rule.
% Folds the rules into a graph.
most_probable_cause_graph(Rule-Rules, Graph0, Graph) :-
  maplist(indexed_rule_intersection(Rule), Rules, CauseSizes),
  [First|Rest] = CauseSizes -> (
    % reversed so that items earlier in the list get priority
    reverse(Rest, Rev),
    foldl(larger_size, Rev, First, Result0),
    fst(Result0, Result),
    add_edge(Rule.i, Result.i, Graph0, Graph)
  ); (
    Graph = Graph0
  ).

% Finds the rule whose RHS with the largest intersection with the LHS of this rulek
most_probable_cause(Rule, Rules, Result) :-
  maplist(rule_intersection(Rule), Rules, CauseSizes),
  [First|Rest] = CauseSizes ->
    % reversed so that items earlier in the list get priority
    (reverse(Rest, Rev), foldl(larger_size, Rev, First, Result0), fst(Result0, Result));
    Result = rule{
      name: no_reason,
      rule: [] -> [],
      text: ['of no known reason']
    }.

larger_size(A-S, B-T, R-U) :-
  M is max(S, T),
  M = S -> (R = A, U = S); (R = B, U = T).
