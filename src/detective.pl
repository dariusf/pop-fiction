
:- use_module(pop_fiction).

main :-
  run,
  halt.

run :-
  %% writeln(hi),
  generate(Causality, Way),
  %% writeln(hi2),
  assoc_to_list(Causality, C),
  writeln(C),
  %% writeln(Way),
  maplist(render_rules(Causality), Way, Rendered),
  print_term(Rendered, []).

generate(Causality, Way) :-
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
  random_story_goal(Initial, dead(_), Rules, Causality, Way).
