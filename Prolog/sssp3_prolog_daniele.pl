% 899826 Roncoroni Daniele

% DA CAPIRE COSA SONO
%:- dynamic heap/2.      
%:- dynamic heap_entry/4.  
%:- dynamic graph/1.   
%:- dynamic vertex/2.  
%:- dynamic edge/3.     
%:- dynamic edge/4.
%:- dynamic dist/3.      
%:- dynamic visited/2.   
%:- dynamic previous/3.  

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