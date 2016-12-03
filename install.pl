install(Pack, _) :-
  always(pack_install(Pack, [interactive(false)])).

always(Goal) :-
  Goal; true.

:-
  Packs = [
    func, % 0.4.2
    lambda % 1.0.0
  ],
  maplist(install, Packs, _),
  halt.
