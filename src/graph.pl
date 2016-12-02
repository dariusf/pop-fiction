
:- module(graph, [add_vertex/3, add_edge/4, graph_to_list/2, vertices/2, edges/2, edges/3]).

:- use_module(library(func)).

graph(Graph) :- empty_assoc(Graph).

add_vertex(Vertex, Graph0, Graph) :-
  \+ get_assoc(Vertex, Graph0, _) ->
    put_assoc(Vertex, Graph0, [], Graph);
    Graph0 = Graph.

add_edge(S, D, Graph0, Graph) :-
  add_vertex(S, Graph0, Graph1),
  add_vertex(D, Graph1, Graph2),
  get_assoc(S, Graph2, SourceTo),
  union(SourceTo, [D], Result),
  put_assoc(S, Graph2, Result, Graph).

graph_to_list(Graph, Result) :-
  assoc_to_list(Graph, Result).

vertices(Graph, Vertices) :-
  assoc_to_keys(Graph, Vertices).

edges(G, E) :-
  vertices(G, V),
  maplist(lookup_edge_(G), V, EdgePairs),
  flatten(EdgePairs, E).

edges(V, G, E) :-
  get_assoc(V, G, E).

lookup_edge_(G, V, E) :-
  edges(V, G, E0),
  maplist(pair_(V), E0, E).

pair_(A, B, A-B).

closure(Graph, Vertex, Result) :-
  edges(Vertex, Graph, E),
  maplist(closure(Graph), E, Related0),
  flatten(Related0, Related),
  append(Related, E, Rest),
  sort([Vertex|Rest], Result).

:- begin_tests(graph).

test(closure) :-
  graph(G0),
  add_edge(a, b, G0, G1),
  add_edge(b, c, G1, G2),
  add_edge(c, d, G2, G),
  closure(G, a, [a, b, c, d]).

test(add_vertex_idempotent) :-
  graph(X),
  add_vertex(a, X, Y),
  add_vertex(a, Y, Z),
  vertices(Z, R),
  R = [a].

test(add_edge_idempotent) :-
  graph(X),
  add_edge(a, b, X, Z),
  add_edge(a, b, Z, B),
  graph_to_list(B, R),
  R = [a-[b], b-[]].

test(edges) :-
  graph(X),
  add_edge(a, b, X, Z),
  add_edge(b, c, Z, B),
  add_edge(a, c, B, A),
  edges(A, Es),
  Es = [a-b, a-c, b-c].

:- end_tests(graph).
