
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
    %% do_form_opinion_like
    [at(C, L), at(D, L), neutral(C, D)] ->
      [at(C, L), at(D, L), philia(C, D)],

    %% do_form_opinion_dislike
    [at(C, L), at(D, L), neutral(C, D)] ->
      [at(C, L), at(D, L), anger(C, D)],

    %% do_compliment_private
    [at(C, L), at(D, L), philia(C, D)] ->
      [at(C, L), at(D, L), philia(C, D), philia(D, C)],

    %% do_compliment_witnessed
    [at(C, L), at(D, L), at(Witness, L), philia(C, D), anger(Witness, D)] ->
      [at(C, L), at(D, L), at(Witness, L), philia(C, D), anger(Witness, D), philia(D, C), anger(Witness, C)],

    %% do_insult_private
    [at(C, L), at(D, L), anger(C, D)] ->
      [at(C, L), at(D, L), anger(C, D), anger(D, C), depressed(D)],

    %% do_insult_witnessed
    [at(C, L), at(D, L), at(Witness, L), anger(C, D), philia(Witness, D)] ->
      [at(C, L), at(D, L), at(Witness, L), anger(C, D), philia(Witness, D), anger(D, C), depressed(D), anger(Witness, C)],

    %% mixed_feelings
    [at(C, L), anger(C, D), philia(C, D)] ->
      [at(C, L), neutral(C, D)],

    %% do_fall_in_love
    [at(C, L), at(D, L1), eros(C, D)] ->
      [at(C, L), at(D, L1), eros(C, D), philia(C, D)],

    %% do_eroticize
    [at(C, L), at(D, L1), philia(C, D), philia(C, D), philia(C, D), philia(C, D)] ->
      [at(C, L), at(D, L1), philia(C, D), eros(C, D)],

    %% do_flirt_ok
    [at(C, L), at(D, L), eros(C, D), unmarried(C), unmarried(D)] ->
      [eros(C, D), eros(D, C), unmarried(C), unmarried(D), at(C, L), at(D, L)],

    %% do_flirt_discreet
    [at(C, L), at(D, L), eros(C, D)] ->
      [eros(C, D), eros(D, C), at(C, L), at(D, L)],

    %% do_flirt_conflict
    [at(C, L), at(D, L), at(E, L), eros(C, D), eros(E, C)] ->
      [eros(C, D), eros(D, C), eros(E, C), anger(E, D), anger(E, C), at(C, L), at(D, L), at(E, L)],

    %% do_marry
    [at(C, L), at(D, L), eros(C, D), philia(C, D), eros(D, C), philia(D, C), unmarried(C), unmarried(D)] ->
      [married(C, D), married(D, C), at(C, L), at(D, L), eros(C, D), eros(D, C), philia(C, D), philia(D, C) ],

    %% do_divorce
    [at(C, L), at(D, L1), married(C, D), married(D, C), anger(C, D), anger(C, D)] ->
      [anger(C, D), anger(D, C), unmarried(C), unmarried(D), at(C, L), at(D, L1)],

    %% do_widow
    [married(C, D), at(C, L), dead(D)] ->
      [unmarried(C), at(C, L)],

    %% do_murder
    [anger(C, D), anger(C, D), anger(C, D), anger(C, D), at(C, L), at(D, L), has(C, weapon)] ->
      [at(C, L), dead(D), murdered(C, D), has(C, weapon)],

    %% do_become_suicidal
    [at(C, L), depressed(C), depressed(C), depressed(C), depressed(C)] ->
      [at(C, L), suicidal(C), wants(C, weapon)],

    %% do_comfort
    [at(C, L), at(D, L), suicidal(D), philia(C, D), philia(D)] ->
      [at(C, L), at(D, L), philia(C, D), philia(D, C), philia(D, C)],

    %% do_suicide
    [at(C, L), suicidal(C), has(C, weapon)] ->
      [dead(C)],

    %% do_grieve
    [at(C, L), philia(C, D), dead(D)] ->
      [at(C, L), depressed(C), depressed(C)],

    %% do_think_vengefully
    [at(C, L), at(Killer, L1), philia(C, Dead), murdered(Killer, Dead)] ->
      [at(C, L), at(Killer, L1), philia(C, Dead), anger(C, Killer), anger(C, Killer)],
    %% do_give
    [at(C, L), at(D, L), has(C, O), wants(D, O), philia(C, D)] ->
      [at(C, L), at(D, L), has(O, C), philia(C, D) -o],

    %% do_steal
    [at(C, L), at(D, L), has(C, O), wants(D, O)] ->
      [at(C, L), at(D, L), has(D, O), anger(C, D)],

    %% do_loot
    [at(C, L), dead(D), has(D, O), wants(C, O)] ->
      [at(C, L), has(C, O)]
  ],
  random_story(Initial, Rules, Way),
  print_term(Way, []),
  halt.
