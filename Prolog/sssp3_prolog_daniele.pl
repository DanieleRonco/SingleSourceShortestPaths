% 899826 Roncoroni Daniele

:- dynamic heap/2.      
:- dynamic heap_entry/4.  
:- dynamic graph/1.   
:- dynamic vertex/2.  
:- dynamic edge/3.     
:- dynamic edge/4.
:- dynamic distance/3.      
:- dynamic visited/2.   
:- dynamic previous/3.  

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