
:- use_module(pop_fiction).

init(Way) :-
  Initial = [
    accessible(mon_house, town),
    accessible(town, mon_house),
    accessible(cap_house, town),
    accessible(town, cap_house),
    at(romeo, town),
    at(montague, mon_house),
    at(capulet, cap_house),
    at(mercutio, town),
    at(nurse, cap_house),
    at(juliet, town),
    at(tybalt, town),
    at(apothecary, town),
    has(tybalt, weapon),
    has(romeo, weapon),
    has(apothecary, weapon),
    unmarried(romeo),
    unmarried(juliet),
    unmarried(nurse),
    unmarried(mercutio),
    unmarried(tybalt),
    unmarried(apothecary),
    anger(montague, capulet),
    anger(capulet, montague),
    anger(tybalt, romeo),
    anger(capulet, romeo),
    anger(montague, tybalt),
    philia(mercutio, romeo),
    philia(romeo, mercutio),
    philia(montague, romeo),
    philia(capulet, juliet),
    philia(juliet, nurse),
    philia(nurse, juliet),
    neutral(nurse, romeo),
    neutral(mercutio, juliet),
    neutral(juliet, mercutio),
    neutral(apothecary, nurse),
    neutral(nurse, apothecary)
  ],
  Rules = [
    rule{
      name: do_form_opinion_like,
      rule: [at(C, L), at(D, L), neutral(C, D)] ->
        [at(C, L), at(D, L), philia(C, D)],
      text: []
    },
    rule{
      name: do_form_opinion_dislike,
      rule: [at(C, L), at(D, L), neutral(C, D)] ->
        [at(C, L), at(D, L), anger(C, D)],
      text: []
    },
    rule{
      name: do_compliment_private,
      rule: [at(C, L), at(D, L), philia(C, D)] ->
        [at(C, L), at(D, L), philia(C, D), philia(D, C)],
      text: []
    },
    rule{
      name: do_compliment_witnessed,
      rule: [at(C, L), at(D, L), at(Witness, L), philia(C, D), anger(Witness, D)] ->
        [at(C, L), at(D, L), at(Witness, L), philia(C, D), anger(Witness, D), philia(D, C), anger(Witness, C)],
      text: []
    },
    rule{
      name: do_insult_private,
      rule: [at(C, L), at(D, L), anger(C, D)] ->
        [at(C, L), at(D, L), anger(C, D), anger(D, C), depressed(D)],
      text: []
    },
    rule{
      name: do_insult_witnessed,
      rule: [at(C, L), at(D, L), at(Witness, L), anger(C, D), philia(Witness, D)] ->
        [at(C, L), at(D, L), at(Witness, L), anger(C, D), philia(Witness, D), anger(D, C), depressed(D), anger(Witness, C)],
      text: []
    },
    rule{
      name: mixed_feelings,
      rule: [at(C, L), anger(C, D), philia(C, D)] ->
        [at(C, L), neutral(C, D)],
      text: []
    },
    rule{
      name: do_fall_in_love,
      rule: [at(C, L), at(D, L1), eros(C, D)] ->
        [at(C, L), at(D, L1), eros(C, D), philia(C, D)],
      text: []
    },
    rule{
      name: do_eroticize,
      rule: [at(C, L), at(D, L1), philia(C, D), philia(C, D), philia(C, D), philia(C, D)] ->
        [at(C, L), at(D, L1), philia(C, D), eros(C, D)],
      text: []
    },
    rule{
      name: do_flirt_ok,
      rule: [at(C, L), at(D, L), eros(C, D), unmarried(C), unmarried(D)] ->
        [eros(C, D), eros(D, C), unmarried(C), unmarried(D), at(C, L), at(D, L)],
      text: []
    },
    rule{
      name: do_flirt_discreet,
      rule: [at(C, L), at(D, L), eros(C, D)] ->
        [at(C, L), at(D, L), eros(C, D), eros(D, C)],
      text: []
    },
    rule{
      name: do_flirt_conflict,
      rule: [at(C, L), at(D, L), at(E, L), eros(C, D), eros(E, C)] ->
        [at(C, L), at(D, L), at(E, L), eros(C, D), eros(D, C), eros(E, C), anger(E, D), anger(E, C)],
      text: []
    },
    rule{
      name: do_marry,
      rule: [at(C, L), at(D, L), eros(C, D), philia(C, D), eros(D, C), philia(D, C), unmarried(C), unmarried(D)] ->
        [at(C, L), at(D, L), eros(C, D), philia(C, D), eros(D, C), married(C, D), married(D, C), philia(D, C) ],
      text: []
    },
    rule{
      name: do_divorce,
      rule: [at(C, L), at(D, L1), married(C, D), married(D, C), anger(C, D), anger(C, D)] ->
        [anger(C, D), anger(D, C), unmarried(C), unmarried(D), at(C, L), at(D, L1)],
      text: []
    },
    rule{
      name: do_widow,
      rule: [married(C, D), at(C, L), dead(D)] ->
        [unmarried(C), at(C, L)],
      text: []
    },
    rule{
      name: do_murder,
      rule: [anger(C, D), anger(C, D), anger(C, D), anger(C, D), at(C, L), at(D, L), has(C, weapon)] ->
        [at(C, L), dead(D), murdered(C, D), has(C, weapon)],
      text: []
    },
    rule{
      name: do_become_suicidal,
      rule: [at(C, L), depressed(C), depressed(C), depressed(C), depressed(C)] ->
        [at(C, L), suicidal(C), wants(C, weapon)],
      text: []
    },
    rule{
      name: do_comfort,
      rule: [at(C, L), at(D, L), suicidal(D), philia(C, D), philia(D)] ->
        [at(C, L), at(D, L), philia(C, D), philia(D, C), philia(D, C)],
      text: []
    },
    rule{
      name: do_suicide,
      rule: [at(C, L), suicidal(C), has(C, weapon)] ->
        [dead(C)],
      text: []
    },
    rule{
      name: do_grieve,
      rule: [at(C, L), philia(C, D), dead(D)] ->
        [at(C, L), depressed(C), depressed(C)],
      text: []
    },
    rule{
      name: do_think_vengefully,
      rule: [at(C, L), at(Killer, L1), philia(C, Dead), murdered(Killer, Dead)] ->
        [at(C, L), at(Killer, L1), philia(C, Dead), anger(C, Killer), anger(C, Killer)],
      text: []
    },
    rule{
      name: do_give,
      rule: [at(C, L), at(D, L), has(C, O), wants(D, O), philia(C, D)] ->
        [at(C, L), at(D, L), has(O, C), philia(C, D) -o],
      text: []
    },
    rule{
      name: do_steal,
      rule: [at(C, L), at(D, L), has(C, O), wants(D, O)] ->
        [at(C, L), at(D, L), has(D, O), anger(C, D)],
      text: []
    },
    rule{
      name: do_loot,
      rule: [at(C, L), dead(D), has(D, O), wants(C, O)] ->
        [at(C, L), has(C, O)],
      text: []
    }
  ],
  random_story(Initial, Rules, Way),
  print_term(Way, []),
  halt.
