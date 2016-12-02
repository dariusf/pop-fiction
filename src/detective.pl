
:- use_module(pop_fiction).
:- use_module(graph).
:- use_module(library(lambda)).
:- use_module(library(func)).
:- use_module(library(list_util)).

main :-
  run,
  halt.

run :-
  state(Initial, Rules),
  generate(Initial, Rules, Way, Graph),

  % Compute causes for each rule
  maplist(get_cause(Way, Graph), range(Way, ~), Causes),

  % Render
  maplist(render_causal, Causes, Rendered),

  % gtrace,
  % Remove answer, and add whatever it was pointing to to the closure
  Last is length(vertices(Graph, ~), ~) - 1,
  exclude(==(Last), minimal_closure(Graph, ~), FactsToDistribute0),
  append(FactsToDistribute0, edges(Last, Graph, ~), FactsToDistribute),

  % TODO prevent self-references?

  % Figure out who is still alive
  people(Initial, People),
  last(Way, KillingRule),
  killed(KillingRule, Dead),
  killer(KillingRule, Killer),
  subtract(People, [Dead], StillAlive),

  % Ensure that there are enough facts to distribute. Failure down the road will cause backtracking
  % anyway, and accounting for it here explicitly seems better.
  length(FactsToDistribute, ~) >= length(StillAlive, ~),

  % Assign rules

  assign_rules(StillAlive, FactsToDistribute, Assignments),

  % Debugging

  % write('facts '), writeln(FactsToDistribute),
  % write('assignments '), writeln(assoc_to_list(Assignments, ~)),
  % print_term(Rendered, []),

  % Prevent unintended backtracking
  !,
  % writeln('UNINTENTIONAL BACKTRACKING?'),

  person_mode(Killer, StillAlive, Way, Graph, Assignments, Rendered).

% Distributes the given rules amongst the people, returning an assoc of assignments.
assign_rules(_, [], Assignments) :- empty_assoc(Assignments).
assign_rules([P|People], [R|Rules], Assignments) :-
  assign_rules(append(People, [P], ~), Rules, Assignments0),
  (get_assoc(P, Assignments0, Current) ->
    put_assoc(P, Assignments0, [R|Current], Assignments)
  ; put_assoc(P, Assignments0, [R], Assignments)).

% Finds the minimal set of nodes such that its transitive closure covers the entire graph.
minimal_closure(Graph, Nodes) :-
  reverse(range(vertices(Graph, ~), ~), Range), % start from the back because edges point backwards
  find_minimal_set(Graph, Range, Nodes).

find_minimal_set(_, [], []).
find_minimal_set(Graph, [Current|ToVisit], [Current|Res]) :-
  graph:closure(Graph, Current, Closure),
  subtract(ToVisit, Closure, Rest),
  find_minimal_set(Graph, Rest, Res).

person_mode(Killer, StillAlive, Rules, Graph, Assignments, Rendered) :-
  choice_prompt('Whom do you wish to speak to?', StillAlive, _, Person) ->
    talk_mode(Killer, StillAlive, Person, Rules, Graph, Assignments, Rendered)
  ; guess_mode(Killer, StillAlive, Rules, Graph, Assignments, Rendered).

guess_mode(Killer, StillAlive, Rules, Graph, Assignments, Rendered) :-
  choice_prompt('Who was the killer?', StillAlive, _, Result) ->
    ((Result = Killer ->
      writeln('You are right!');
      write('The killer got away (it was '), write(Killer), writeln(')')),
      print_term(Rendered, []), write('\n')
    ); person_mode(Killer, StillAlive, Rules, Graph, Assignments, Rendered).

% Takes a list of things and prompts for a choice of one of them. Fails if
% nothing is chosen, otherwise is forced to always return a valid choice.
choice_prompt(Question, Items, Index, Result) :-
  append(Items, ['Back\n'], Items1),
  length(Items1, L),
  range(1, ~ is L + 1, R),
  writeln(Question), write('\n'),
  maplist(\N^P^_^(write(N), write('. '), writeln(P)), R, Items1, _),
  (getn(Index), Index =< L ->
    % Fail if the last item is chosen
    Index < L, nth1(Index, Items1, Result)
  ; otherwise ->
    writeln('Please enter a number within range\n'),
    choice_prompt(Question, Items, Index, Result)
  ).

talk_mode(Killer, StillAlive, Person, Rules, Graph, Assignments, Rendered) :-
  % writeln('talk mode'),
  get_assoc(Person, Assignments, Assigned),
  maplist(Rules+\A^Res^nth0(A, Rules, Res), Assigned, RulesToTalkAbout),
  maplist(get_topic, RulesToTalkAbout, Topics),
  (choice_prompt('What would you like to talk about?', Topics, Result, _) ->
    topic_mode(Killer, StillAlive, Person, nth1(Result, Assigned, ~), Rules, Graph, Assignments, Rendered)
  ; person_mode(Killer, StillAlive, Rules, Graph, Assignments, Rendered)).

get_topic(X, X.topic).

topic_mode(Killer, StillAlive, Person, N, Rules, Graph, Assignments, Rendered) :-
  % Debugging
  % writeln('topic mode'),
  % writeln(N),
  % writeln(graph_to_list(Graph, ~)),
  nth0(N, Rules, Rule),
  (edges(N, Graph, [Next]) -> Choices = ['Why?']; Choices = []),
  atomic_list_concat([Person, ': ', render_rule(Rule, ~)], Speech),
  (choice_prompt(Speech, Choices, _, _) ->
    % this has to succeed since there must be something there
    topic_mode(Killer, StillAlive, Person, Next, Rules, Graph, Assignments, Rendered)
  ; talk_mode(Killer, StillAlive, Person, Rules, Graph, Assignments, Rendered)).

getn(S) :- gets(S0), string_to_num(S0, S).

gets(S) :-
  read_string(user_input, '\n', '\r', _, S).

string_to_num(S, N) :-
  catch(number_codes(N, S), _, fail).

people(Initial, People) :-
  findall(X, member(person(X), Initial), People).

killed(Rule, Dead) :-
  (_->R) = Rule.rule,
  once(member(dead(Dead), R)).

killer(Rule, Dead) :-
  (_->R) = Rule.rule,
  once(member(person(Dead), R)).

render_causal(Rule-Cause, Res) :-
  render_rule(Rule, Rule1),
  render_rule(Cause, Cause1),
  atomic_list_concat([Rule1, Cause1], ' because ', Res).

get_cause(Rules, G, V0, V-Cause) :-
  nth0(V0, Rules, V),
  edges(V0, G, E),
  ([C] = E ->
    nth0(C, Rules, Cause)
  ; Cause =
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
    % TODO all but the first two rules don't really admit goood causal intersections
    rule{
      name: like,
      rule: [person(A), person(B)] ->
        [person(A), person(B), like(A, B)],
      text: [A, 'began to like', B],
      topic: love
    },
    rule{
      name: hate,
      rule: [person(A), person(B)] ->
        [person(A), person(B), hate(A, B)],
      text: [A, 'began to hate', B],
      topic: hatred
    },
    rule{
      name: steal,
      rule: [person(A), person(B), has(B, I), hate(A, B)] ->
        [person(A), person(B), has(A, I), hate(A, B), hate(B, A)],
      text: [A, 'stole', I, 'from', B],
      topic: theft
    },
    rule{
      name: give,
      rule: [person(A), person(B), has(A, I), like(A, B)] ->
        [person(A), person(B), has(B, I), like(A, B), like(B, A)],
      text: [A, 'gave', I, 'to', B],
      topic: kindness
    },
    rule{
      name: kill,
      rule: [person(A), person(B), hate(A, B), hate(A, B)] ->
        [person(A), dead(B)],
      text: [A, 'killed', B, 'out of spite'],
      topic: murder
    },
    rule{
      name: jealousy,
      rule: [person(A), person(B), person(C), like(A, B), like(A, B)] ->
        [person(A), person(B), person(C), like(A, B), like(A, B), hate(C, B), hate(C, B)],
      text: [C, 'became jealous of', A, 'and', B],
      topic: envy
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
    (reverse(Rest, Rev), foldl(larger_size, Rev, First, Result0), fst(Result0, Result))
  ; Result = rule{
      name: no_reason,
      rule: [] -> [],
      text: ['of no known reason']
    }.

larger_size(A-S, B-T, R-U) :-
  M is max(S, T),
  M = S -> (R = A, U = S); (R = B, U = T).
