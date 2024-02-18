%%% -*- Mode: Prolog -*-

%%% Marco Tantardini 869525

%%% MINHEAP

:- dynamic heap/2.

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


%%% end of file -- sssp.pl --
