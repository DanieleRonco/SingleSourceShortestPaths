%%% 899826 Roncoroni Daniele
%%% 869525 Marco Tantardini

:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic edge/3.
:- dynamic edge/4.
:- dynamic distance/3.
:- dynamic visited/2.
:- dynamic previous/3.

%%% Interfaccia Prolog per la Manipolazione dei Dati

new_graph(G) :-
	graph(G),
	!.
new_graph(G) :-
	assert(graph(G)),
	!.

delete_graph(G) :-
	graph(G),
	retractall(edge(G, _, _, _)),
	retractall(vertex(G, _)),
	retractall(graph(G)),
	!.

new_vertex(G, V) :-
	nonvar(G),
	nonvar(V),
	graph(G),
	vertex(G, V),
	!.
new_vertex(G, V) :-
	assert(vertex(G, V)),
	!.

vertices(G, Vs) :-
	nonvar(G),
	graph(G),
	findall(V, vertex(G, V), Weight),
	Vs = Weight.

list_vertices(G) :-
	nonvar(G),
	graph(G),
	listing(vertex(G, _)).

new_edge(G, U, V) :-
	new_edge(G, U, V, 1),
	!.
new_edge(G, U, V, Weight) :-
	nonvar(G),
	nonvar(U),
	nonvar(V),
	nonvar(Weight),
	graph(G),
	edge(G, U, V, _),
	retractall(edge(G, U, V, _)),
	new_edge(G, U, V, Weight),
	!.
new_edge(G, U, V, Weight) :-
	assert(edge(G, U, V, Weight)),
	!.

edges(G, Es) :-
	nonvar(G),
	graph(G),
	Edge = edge(G, _, _, _),
	findall(Edge, Edge, Edges),
	Es = Edges.

neighbors(G, V, Ns) :-
	nonvar(G),
	nonvar(V),
	graph(G),
	vertex(G, V),
	Neighbor = edge(G, V, _, _),
	findall(Neighbor, Neighbor, Neighbors),
	Ns = Neighbors.

list_edges(G) :-
	nonvar(G),
	graph(G),
	listing(edge(G, _, _, _)).

list_graph(G) :-
	nonvar(G),
	graph(G),
	list_vertices(G),
	list_edges(G).

%%% MinHeap

%%% new_heap/1
new_heap(H) :-
    heap(H, _),
    !.
new_heap(H) :-
    assert(heap(H, 0)),
    !.

%%% delete_heap/1
delete_heap(H) :-
    heap(H, 0),
    retract(heap(H, 0)),
    !.
delete_heap(H) :-
    heap(H, _),
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)),
    !.

%%% heap_size/2
heap_size(H, S) :-
    heap(H, S),
    !.

%%% empty/1
empty(H) :-
    nonvar(H),
    heap(H, 0),
    !.

%%% not_empty/1
not_empty(H) :-
    nonvar(H),
    heap(H, S),
    S > 0.

%%% head/3
head(H, K, V) :-
    nonvar(H),
    heap(H, _),
    heap_entry(H, 1, K, V),
    !.

%%% insert/3
insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    nonvar(V),
    heap(H, _),
    not_empty(H),
    heap_entry(H, _, K, V),
    !.
insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    nonvar(V),
    heap(H, _),
    not_empty(H),
    heap_entry(H, _, OldK, V),
    modify_key(H, K, OldK, V),
    !.
insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    nonvar(V),
    heap(H, S),
    Stemp is S + 1,
    \+ heap_entry(H, _, K, V),
    retract(heap(H, S)),
    assert(heap(H, Stemp)),
    assert(heap_entry(H, Stemp, K, V)),
    propertyHeap(H, Stemp),
    !.

%%% extract/3
extract(H, K, _) :-
    nonvar(H),
    nonvar(K),
    empty(H),
    !.
extract(H, K, V) :-
    nonvar(H),
    nonvar(V),
    heap(H, _),
    not_empty(H),
    \+ head(H, K, V),
    !.
extract(H, K, V) :-
    nonvar(H),
    nonvar(V),
    heap(H, S),
    not_empty(H),
    S = 1,
    heap_entry(H, S, K, V),
    Stemp is S - 1,
    retract(heap_entry(H, S, K, V)),
    retract(heap(H, S)),
    assert(heap(H, Stemp)),
    !.
extract(H, K, V) :-
    nonvar(H),
    nonvar(V),
    heap(H, S),
    not_empty(H),
    Stemp is S - 1,
    head(H, K, V),
    heap_entry(H, S, KL, VL),
    change(H, 1, K, V, S, KL, VL),
    retract(heap_entry(H, S, K, V)),
    retract(heap(H, S)),
    assert(heap(H, Stemp)),
    heapify(H, 1),
    !.

%%% modify_key/4
modify_key(H, NK, OK, V) :-
    nonvar(H),
    nonvar(NK),
    nonvar(OK),
    nonvar(V),
    NK = OK,
    heap_entry(H, _, OK, V),
    !.
modify_key(H, NK, OK, V) :-
    nonvar(H),
    nonvar(NK),
    nonvar(OK),
    nonvar(V),
    heap_entry(H, P, OK, V),
    NK < OK,
    retract(heap_entry(H, P, OK, V)),
    assert(heap_entry(H, P, NK, V)),
    propertyHeap(H, P),
    !.
modify_key(H, NK, OK, V) :-
    nonvar(H),
    nonvar(NK),
    nonvar(OK),
    nonvar(V),
    heap_entry(H, P, OK, V),
    NK > OK,
    retract(heap_entry(H, P, OK, V)),
    assert(heap_entry(H, P, NK, V)),
    propertyHeap(H, P),
    !.

%%% list_heap/1
list_heap(H) :-
    nonvar(H),
    listing(heap_entry(H, _, _, _)).

%%% change/7
change(H, Pold, Kold, Vold, Pnew, Knew, Vnew) :-
    nonvar(H),
    nonvar(Pold),
    nonvar(Kold),
    nonvar(Vold),
    nonvar(Pnew),
    nonvar(Knew),
    nonvar(Vnew),
    heap_entry(H, Pold, Kold, Vold),
    heap_entry(H, Pnew, Knew, Vnew),
    retract(heap_entry(H, Pold, Kold, Vold)),
    retract(heap_entry(H, Pnew, Knew, Vnew)),
    assert(heap_entry(H, Pold, Kold, Vold)),
    assert(heap_entry(H, Pnew, Knew, Vnew)),
    !.

%%% min/4
min(H, P1, P2, Min) :-
    nonvar(H),
    nonvar(P1),
    nonvar(P2),
    heap(H, S),
    S >= P1,
    S >= P2,
    heap_entry(H, P1, K1, _),
    heap_entry(H, P2, K2, _),
    K1 < K2,
    Min is P1,
    !.
min(H, P1, P2, Min) :-
    nonvar(H),
    nonvar(P1),
    nonvar(P2),
    heap(H, S),
    S >= P1,
    S >= P2,
    heap_entry(H, P1, Kone, _),
    heap_entry(H, P2, Ktwo, _),
    Kone >= Ktwo,
    Min is P2,
    !.
min(H, P1, P2, P3, Min) :-
    nonvar(H),
    nonvar(P1),
    nonvar(P2),
    nonvar(P3),
    heap(H, S),
    S >= P1,
    S >= P2,
    S >= P3,
    min(H, P1, P2, Min1),
    min(H, P2, P3, Min2),
    heap_entry(H, Min1, Kone, _),
    heap_entry(H, Min2, Ktwo, _),
    Kone < Ktwo,
    Min is Min1,
    !.
min(H, P1, P2, P3, Min) :-
    nonvar(H),
    nonvar(P1),
    nonvar(P2),
    nonvar(P3),
    heap(H, S),
    S >= P1,
    S >= P2,
    S >= P3,
    min(H, P1, P2, Min1),
    min(H, P2, P3, Min2),
    heap_entry(H, Min1, Kone, _),
    heap_entry(H, Min2, Ktwo, _),
    Kone >= Ktwo,
    Min is Min2,
    !.

%%% order_heap/2
order_heap(H, _) :-
    nonvar(H),
    empty(H),
    !.
order_heap(H, _) :-
    nonvar(H),
    not_empty(H),
    heap(H, 1),
    !.
order_heap(H, N) :-
    nonvar(H),
    not_empty(H),
    heap(H, S),
    S < N,
    !.
order_heap(H, N) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    S >= N,
    L is 2 * N,
    L > S,
    R is ((2 * N) +1),
    R > S,
    !.
order_heap(H, N) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    S >= N,
    L is 2 * N,
    S >= L,
    R is ((2 * N) + 1),
    R > S,
    heap_entry(H, L, _, _),
    heap_entry(H, L, _, _),
    heap_entry(H, N, _, _),
    min(H, N, L, Min),
    N = Min,
    order_heap(H, L),
    !.
order_heap(H, N) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    S >= N,
    L is 2 * N,
    S >= L,
    R is ((2 * N) + 1),
    R > S,
    heap_entry(H, L, _, _),
    heap_entry(H, N, _, _),
    min(H, N, L, Min),
    N \= Min,
    heap_entry(H, Min, Km, Vm),
    heap_entry(H, N, Kn, Vn),
    change(H, Min, Km, Vm, N, Kn, Vn),
    order_heap(H, Min),
    !.
order_heap(H, N) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    S >= N,
    L is 2 * N,
    S >= L,
    R is ((2 * N) + 1),
    R > S,
    heap_entry(H, L, _, _),
    heap_entry(H, R, _, _),
    heap_entry(H, N, _, _),
    min(H, N, L, R, Min),
    N = Min,
    !.
order_heap(H, N) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    S >= N,
    L is 2 * N,
    S >= L,
    R is ((2 * N) + 1),
    R >= S,
    heap_entry(H, L, _, _),
    heap_entry(H, R, _, _),
    heap_entry(H, N, _, _),
    min(H, N, L, R, Min),
    N \= Min,
    heap_entry(H, Min, Km, Vm),
    heap_entry(H, N, Kn, Vn),
    change(H, Min, Km, Vm, N, Kn, Vn),
    order_heap(H, Min),
    !.

%%% propertyHeap/2
propertyHeap(H, _) :-
    nonvar(H),
    heap(H, _),
    empty(H),
    !.
propertyHeap(H, _) :-
    nonvar(H),
    heap(H, S),
    not_empty(H),
    S = 1,
    !.
propertyHeap(H, N) :-
    nonvar(H),
    heap(H, S),
    not_empty(H),
    S \= 1,
    N = 1,
    !.
propertyHeap(H, N) :-
    nonvar(H),
    heap(H, S),
    not_empty(H),
    S \= 1,
    N \= 1,
    heap_entry(H, N, _, _),
    F is floor(N / 2),
    heap_entry(H, F, _, _),
    min(H, N, F, M),
    M = F,
    !.
propertyHeap(H, N) :-
    nonvar(H),
    heap(H, S),
    not_empty(H),
    S \= 1,
    N \= 1,
    heap_entry(H, N, Kn, Vn),
    F is floor(N / 2),
    heap_entry(H, F, Kf, Vf),
    min(H, N, F, M),
    M \= F,
    change(H, N, Kn, Vn, F, Kf, Vf),
    propertyHeap(H, F),
    !.

%%% SSSP in Prolog

change_distance(G, V, NewDist) :-
	nonvar(G),
	nonvar(V),
	nonvar(NewDist),
	graph(G),
	vertex(G, V),
	retractall(distance(G, V, _)),
	assert(distance(G, V, NewDist)),
	!.

change_previous(G, V, U) :-
	nonvar(G),
	nonvar(V),
	nonvar(U),
	graph(G),
	vertex(G, V),
	vertex(G, U),
	retractall(previous(G, V, _)),
	assert(previous(G, V, U)),
	!.

dijkstra_sssp(G, Source) :-
	nonvar(G),
	nonvar(Source),
	graph(G),
	vertex(G, Source),
	new_heap(h),
	retractall(previous(G, _, _)),
	retractall(distance(G, _, _)),
	retractall(visited(G, _)),
	vertices(G, Vs),
	initialise_source(G, Source, h, Vs),
	change_distance(G, Source, 0),
	max_int(Max),
	modify_key(h, 0, Max, Source),
	dijkstra_sssp_helper(G, Source, h),
	delete_heap(h),
	!.

initialise_source(G, Source, Heap, Vs) :-
	nonvar(G),
	nonvar(Source),
	nonvar(Heap),
	nonvar(Vs),
	graph(G),
	vertex(G, Source),
	heap(Heap, _),
	Vs = [],
	!.
initialise_source(G, Source, H, [V | Vs]) :-
	nonvar(G),
	nonvar(Source),
	nonvar(H),
	nonvar(V),
	nonvar(Vs),
	graph(G),
	vertex(G, Source),
	heap(H, _),
	[V | Vs] \= [],
	vertex(G, V),
	max_int(Max),
	insert(H, Max, V),
	assert(distance(G, V, Max)),
	assert(previous(G, V, null)),
	initialise_source(G, Source, H, Vs),
	!.

max_int(Max) :-
	Max is 40000,
	!.

dijkstra_sssp_helper_neighbor(G, V, H, Ns) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	empty(H),
	!.
dijkstra_sssp_helper_neighbor(G, V, H, Ns) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	Ns = [],
	!.
dijkstra_sssp_helper_neighbor(G, V, H, [N | Ns]) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(N),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	[N | Ns] \= [],
	edge(G, V, U, _) = N,
	distance(G, V, _),
	\+ heap_entry(H, _, _, U),
	dijkstra_sssp_helper_neighbor(G, V, H, Ns),
	!.
dijkstra_sssp_helper_neighbor(G, V, H, [N | Ns]) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(N),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	[N | Ns] \= [],
	edge(G, V, U, Weight) = N,
	distance(G, V, D),
	heap_entry(H, _, OldKey, U),
	NewKey is (D + Weight),
	OldKey > NewKey,
	modify_key(H, NewKey, OldKey, U),
	change_distance(G, U, NewKey),
	change_previous(G, U, V),
	dijkstra_sssp_helper_neighbor(G, V, H, Ns),
	!.
dijkstra_sssp_helper_neighbor(G, V, H, [N | Ns]) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	nonvar(N),
	nonvar(Ns),
	graph(G),
	vertex(G, V),
	heap(H, _),
	not_empty(H),
	[N | Ns] \= [],
	edge(G, V, U, Weight) = N,
	distance(G, V, D),
	heap_entry(H, _, OldKey, U),
	NewKey is (D + Weight),
	NewKey >= OldKey,
	dijkstra_sssp_helper_neighbor(G, V, H, Ns),
	!.

dijkstra_sssp_helper(G, V, H) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	graph(G),
	vertex(G, V),
	heap(H, _),
	empty(H),
	!.
dijkstra_sssp_helper(G, V, H) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	graph(G),
	vertex(G, V),
	heap(H, S),
	not_empty(H),
	S = 1,
	neighbors(G, V, Ns),
	dijkstra_sssp_helper_neighbor(G, V, H, Ns),
	assert(visited(G, V)),
	distance(G, V, D),
	extract(H, D, V),
	!.
dijkstra_sssp_helper(G, V, H) :-
	nonvar(G),
	nonvar(V),
	nonvar(H),
	graph(G),
	vertex(G, V),
	heap(H, S),
	not_empty(H),
	S \= 1,
	neighbors(G, V, Ns),
	dijkstra_sssp_helper_neighbor(G, V, H, Ns),
	assert(visited(G, V)),
	distance(G, V, D),
	extract(H, D, V),
	head(H, _, U),
	dijkstra_sssp_helper(G, U, H),
	!.

shortest_path(G, Source, V, Path) :-
	nonvar(G),
	nonvar(Source),
	nonvar(V),
	graph(G),
	vertex(G, Source),
	vertex(G, V),
	Source = V,
	Path = [],
	!.
shortest_path(G, Source, V, Path) :-
	nonvar(G),
	nonvar(Source),
	nonvar(V),
	graph(G),
	vertex(G, Source),
	vertex(G, V),
	Source \= V,
	previous(G, V, U),
	edge(G, U, V, Weight),
	shortest_path(G, Source, U, Ptmp),
	append(Ptmp, [edge(G, U, V, Weight)], Path),
	!.
