
:- use_module(pop_fiction).

main :-
  run,
  halt.

run :-
  generate(Way, Causes),
  maplist(render_rules, Way, Rendered),
  maplist(render_rules, Causes, RenderedCauses),
  maplist(concat_cause, Rendered, RenderedCauses, Res),
  print_term(Res, []).

concat_cause(Rule, Cause, Res) :-
  atomic_list_concat([Rule, Cause], ' because ', Res).

generate(Way, Causes) :-
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
  ],
  random_story_goal(Initial, dead(_), Rules, Way),
  all_before(Way, AllBefore),
  maplist(most_probable_cause, Way, AllBefore, Causes).

all_before([], []).
all_before([_], [[]]) :- !.
all_before([X|Xs], [[]|Ys]) :-
  all_before(Xs, Zs),
  maplist(cons(X), Zs, Ys).

cons(X, Xs, [X|Xs]).
fst(A-_, A).
snd(_-B, B).

rule_intersection(Rule, Cause, Cause-Size) :-
  (L->_) = Rule.rule,
  (_->Cr) = Cause.rule,
  intersection(L, Cr, R),
  length(R, Size).

% Finds the rule whose RHS with the largest intersection with the LHS of this rulek
most_probable_cause(Rule, Rules, Result) :-
  maplist(rule_intersection(Rule), Rules, CauseSizes),
  [First|Rest] = CauseSizes ->
    % reverse this so that items earlier in the list get priority
    (reverse(Rest, Rev), foldl(larger_size, Rev, First, Result0), fst(Result0, Result));
    Result = rule{
      name: no_reason,
      rule: [] -> [],
      text: ['of no known reason']
    }.

larger_size(A-S, B-T, R-U) :-
  M is max(S, T),
  M = S -> (R = A, U = S); (R = B, U = T).

