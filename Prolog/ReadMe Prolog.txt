Componenti del gruppo:

899826 Roncoroni Daniele
869525 Marco Tantardini

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

