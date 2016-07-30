
:- use_module(pop_fiction).

main :-
  run,
  halt.

run :-
  generate(Way),
  maplist(render_rules, Way, Rendered),
  print_term(Rendered, []).

generate(Way) :-
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
      text: [C, 'took a fancy to', D]
    },
    rule{
      name: do_form_opinion_dislike,
      rule: [at(C, L), at(D, L), neutral(C, D)] ->
        [at(C, L), at(D, L), anger(C, D)],
      text: [C, 'felt anger towards', D]
    },
    rule{
      name: do_compliment_private,
      rule: [at(C, L), at(D, L), philia(C, D)] ->
        [at(C, L), at(D, L), philia(C, D), philia(D, C)],
      text: [C, 'paid', D, 'a compliment, causing', D, 'to blush']
    },
    rule{
      name: do_compliment_witnessed,
      rule: [at(C, L), at(D, L), at(Witness, L), philia(C, D), anger(Witness, D)] ->
        [at(C, L), at(D, L), at(Witness, L), philia(C, D), anger(Witness, D), philia(D, C), anger(Witness, C)],
      text: [C, 'paid', D, 'a compliment.', D, 'blushed, but', Witness, 'saw this and felt anger towards', C, 'and', D]
    },
    rule{
      name: do_insult_private,
      rule: [at(C, L), at(D, L), anger(C, D)] ->
        [at(C, L), at(D, L), anger(C, D), anger(D, C), depressed(D)],
      text: [C, 'lashed out at', D, 'and hurt', D, 'deeply']
    },
    rule{
      name: do_insult_witnessed,
      rule: [at(C, L), at(D, L), at(Witness, L), anger(C, D), philia(Witness, D)] ->
        [at(C, L), at(D, L), at(Witness, L), anger(C, D), philia(Witness, D), anger(D, C), depressed(D), anger(Witness, C)],
      text: [C, 'lashed out at', D, 'and hurt', D, 'deeply.', Witness, 'saw, and began to resent', C]
    },
    rule{
      name: mixed_feelings,
      rule: [at(C, L), anger(C, D), philia(C, D)] ->
        [at(C, L), neutral(C, D)],
      text: [C, 'felt a confusing mix of anger and philia towards', D]
    },
    rule{
      name: do_fall_in_love,
      rule: [at(C, L), at(D, L1), eros(C, D)] ->
        [at(C, L), at(D, L1), eros(C, D), philia(C, D)],
      text: [C, 'developed strong feelings for', D]
    },
    rule{
      name: do_eroticize,
      rule: [at(C, L), at(D, L1), philia(C, D), philia(C, D), philia(C, D), philia(C, D)] ->
        [at(C, L), at(D, L1), philia(C, D), eros(C, D)],
      text: [C, 'began to desire', D]
    },
    rule{
      name: do_flirt_ok,
      rule: [at(C, L), at(D, L), eros(C, D), unmarried(C), unmarried(D)] ->
        [eros(C, D), eros(D, C), unmarried(C), unmarried(D), at(C, L), at(D, L)],
      text: [C, 'and', D, 'were unmarried, and that led to no shortage of flirting between them']
    },
    rule{
      name: do_flirt_discreet,
      rule: [at(C, L), at(D, L), eros(C, D)] ->
        [at(C, L), at(D, L), eros(C, D), eros(D, C)],
      text: [C, 'and', D, 'had a playful relationship']
    },
    rule{
      name: do_flirt_conflict,
      rule: [at(C, L), at(D, L), at(E, L), eros(C, D), eros(E, C)] ->
        [at(C, L), at(D, L), at(E, L), eros(C, D), eros(D, C), eros(E, C), anger(E, D), anger(E, C)],
      text: [C, 'and', D, 'constantly flirting led', E, 'to resent them both']
    },
    rule{
      name: do_marry,
      rule: [at(C, L), at(D, L), eros(C, D), philia(C, D), eros(D, C), philia(D, C), unmarried(C), unmarried(D)] ->
        [at(C, L), at(D, L), eros(C, D), philia(C, D), eros(D, C), married(C, D), married(D, C), philia(D, C) ],
      text: ['Deeply in love,', C, 'and', D, 'decided to marry']
    },
    rule{
      name: do_divorce,
      rule: [at(C, L), at(D, L1), married(C, D), married(D, C), anger(C, D), anger(C, D)] ->
        [at(C, L), at(D, L1), unmarried(C), unmarried(D), anger(C, D), anger(D, C)],
      text: ['Filled with resentment for each other,', C, 'and', D, 'could no longer stay married']
    },
    rule{
      name: do_widow,
      rule: [at(C, L), married(C, D), dead(D)] ->
        [at(C, L), unmarried(C)],
      text: ['Now that', D, 'was dead,', C, 'was alone']
    },
    rule{
      name: do_murder,
      rule: [at(C, L), at(D, L), anger(C, D), anger(C, D), anger(C, D), anger(C, D), has(C, weapon)] ->
        [at(C, L), dead(D), murdered(C, D), has(C, weapon)],
      text: [C, 'was blinded with rage for', D, 'and murdered', D, 'at their next confrontation']
    },
    rule{
      name: do_become_suicidal,
      rule: [at(C, L), depressed(C), depressed(C), depressed(C), depressed(C)] ->
        [at(C, L), suicidal(C), wants(C, weapon)],
      text: [C, 'began to think thoughts of suicide']
    },
    rule{
      name: do_comfort,
      rule: [at(C, L), at(D, L), suicidal(D), philia(C, D)] ->
        [at(C, L), at(D, L), philia(C, D), philia(D, C)],
      text: [C, 'considered suicide, but decided against it when comforted by', D]
    },
    rule{
      name: do_suicide,
      rule: [at(C, L), suicidal(C), has(C, weapon)] ->
        [dead(C)],
      text: ['Weapon in hand,', C, 'committed suicide']
    },
    rule{
      name: do_grieve,
      rule: [at(C, L), philia(C, D), dead(D)] ->
        [at(C, L), depressed(C), depressed(C)],
      text: ['Now that', D, 'was dead,', C, 'was deeply saddened, falling into depression']
    },
    rule{
      name: do_think_vengefully,
      rule: [at(C, L), at(Killer, L1), philia(C, Dead), murdered(Killer, Dead)] ->
        [at(C, L), at(Killer, L1), philia(C, Dead), anger(C, Killer), anger(C, Killer)],
      text: [C, 'wanted vengeance against', Killer, 'for the death of', Dead]
    },
    rule{
      name: do_give,
      rule: [at(C, L), at(D, L), has(C, O), wants(D, O), philia(C, D)] ->
        [at(C, L), at(D, L), has(D, O), philia(C, D)],
      text: [D, 'desired', O, 'and', C, 'willingly handed it over']
    },
    rule{
      name: do_steal,
      rule: [at(C, L), at(D, L), has(C, O), wants(D, O)] ->
        [at(C, L), at(D, L), has(D, O), anger(C, D)],
      text: [D, 'stole', O, 'from', C, 'and angered the latter']
    },
    rule{
      name: do_loot,
      rule: [at(C, L), dead(D), has(D, O), wants(C, O)] ->
        [at(C, L), has(C, O)],
      text: ['Now that', D, 'was dead,', C, 'was free to take', O]
    }
  ],
  random_story(Initial, Rules, Way).
