Componenti del gruppo:

899826 Roncoroni Daniele
869525 Marco Tantardini

Algoritmo Dijkstra in Prolog

%%% Interfaccia Prolog per la Manipolazione dei Dati

Ogni grafo è definito da fatti:
	graph(G).
	Il grafo G

	vertex(G, V).
	Il vertice V del grafo G

	edge(G, V, U, Weight)
	Arco tra i vertici V e U, con peso non negativo.

new_graph(G).
Crea un nuovo grafo G nel database Prolog.

delete_graph(G).
Rimuove un grafo G dal database Prolog (con relativi vertici ed archi).

new_vertex(G, V).
Crea un nuovo vertice V nel grafo G.

vertices(G, Vs).
Vero se Vs contiene tutti i vertici di G.

list_vertices(G).
Stampa la lista dei vertici del grafo G.

new_edge(G, V, U, Weight).
Crea un nuovo arco nel grafo G tra i vertici V e U, con peso non negativo.

edges(G, Es).
Vero quando Es è una lista di tutti gli archi in G.

neighbors(G, V, Ns).
Vero quando V è un vertice di G e Ns è una lista di archi che portano ai vertici N adiacenti a V.

list_edges(G).
Stampa la lista degli archi del grafo G.

list_graph(G).
Stampa la lista dei vertici e degli archi del grafo G.

%%% Minheap 

new_heap(H).
Questo predicato inserisce un nuovo heap nella base-dati Prolog.

delete_heap(H).
Rimuove tutto lo heap dalla base-dati Prolog.

heap_size(H, S).
Questo predicato è vero quanto S è la dimensione corrente dello heap.

empty(H).
Questo predicato è vero quando lo heap H non contiene elementi.

not_empty(H).
Questo predicato è vero quando lo heap H contiene almeno un elemento.

head(H, K, V).
Il predicato head/3 è vero quando l'elemento dello heap H con chiave minima K è V.

insert(H, K, V).
Il predicato insert/3 è vero quando l'elemento V è inserito nello heap H con chiave K.

extract(H, K, V).
Il predicato extract/3 è vero quando la coppia K, V con K minima, è rimossa dallo heap H.

modify_key(H, NewKey, OldKey, V).
Il predicato modify_key/4 è vero quando la chiave OldKey (associata al valore V) è sostituita
da NewKey.

list_heap(H).
Il predicato richiama listing/1 per stampare sulla console Prolog lo stato interno dello heap.

%%% SSSP in Prolog

visited(G, V).
Vero quando V è un vertice di G e V risulta "visitato".

distance(G, V, D).
Vero quando V è un vertice di G e la distanza minima del vertice V dalla sorgente è D.

previous(G, V, U).
Vero quando V ed U sono vertici di G e U è il vertice precedente a V nel cammino minimo dalla sorgente a V.

change_distance(G, V, NewDist).
Ha sempre successo, con effetti collaterali. Ritira dalla base di dati tutte le istanze di distance(G, V, _) e asserisce distance(G, V, NewDist).

change_previous(G, V, U).
Ha sempre successo, con effetti collaterali. Ritira dalla base di dati tutte le istanze di previous(G, V, _) e asserisce previous(G, V, U).

dijkstra_sssp(G, Source).
Ha sempre successo con un effetto collaterale. Al termine, la base di dati contiene predicati distance(G, V, D) per ogni V di G ed i predicati previous(G, V, U) e visited(V)
per ogni V, ottenuti durante le iterazioni dell'algoritmo di Dijkstra.

dijkstra_sssp_helper(G, V, H).
Aiuta nell'implementazione dell'algoritmo di Dijkstra, gestendo i casi in cui l'heap H è vuoto, il nodo di partenza ha distanza 1 o il nodo di partenza ha distanza diversa da 1.

dijkstra_sssp_helper_neighbor(G, V, H, Ns).
Aiuta nell'implementazione dell'algoritmo di Dijkstra, gestendo la vuotezza o non vuotezza dell'heap H, la presenza di vicini del nodo corrente e l'aggiornamtno dei valori della distanza.

shortest_path(G, Source, V, Path).
Vero quando Path è una lista di archi.