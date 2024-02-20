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
    new_entry(H, 1, K, V),
    !.

%%% insert/3
insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    nonvar(V),
    heap(H, _),
    not_empty(H),
    new_entry(H, _, K, V),
    !.
insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    nonvar(V),
    heap(H, _),
    not_empty(H),
    new_entry(H, _, OldK, V),
    modify_key(H, K, OldK, V),
    !.
insert(H, K, V) :-
    nonvar(H),
    nonvar(K),
    nonvar(V),
    heap(H, S),
    Stemp is S + 1,
    \+ new_entry(H, _, K, V),
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
    new_entry(H, S, K, V),
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
    new_entry(H, S, KL, VL),
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
    new_entry(H, _, OK, V),
    !.
modify_key(H, NK, OK, V) :-
    nonvar(H),
    nonvar(NK),
    nonvar(OK),
    nonvar(V),
    new_entry(H, P, OK, V),
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
    new_entry(H, P, OK, V),
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
    new_entry(H, Pold, Kold, Vold),
    new_entry(H, Pnew, Knew, Vnew),
    retract(new_entry(H, Pold, Kold, Vold)),
    retract(new_entry(H, Pnew, Knew, Vnew)),
    assert(new_entry(H, Pold, Kold, Vold)),
    assert(new_entry(H, Pnew, Knew, Vnew)),
    !.

%%% min/4
min(H, P1, P2, Min) :-
    nonvar(H),
    nonvar(P1),
    nonvar(P2),
    heap(H, S),
    S >= P1,
    S >= P2,
    new_entry(H, P1, K1, _),
    new_entry(H, P2, K2, _),
    Kone < Ktwo,
    Min is P1,
    !.
min(H, P1, P2, Min) :-
    nonvar(H),
    nonvar(P1),
    nonvar(P2),
    heap(H, S),
    S >= P1,
    S >= P2,
    new_entry(H, P1, Kone, _),
    new_entry(H, P2, Ktwo, _),
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
    new_entry(H, Min1, Kone, _),
    new_entry(H, Min2, Ktwo, _),
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
    new_entry(H, Min1, Kone, _),
    new_entry(H, Min2, Ktwo, _),
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
    R is ((2 * Node) +1),
    R > S,
    !.
order_heap(H, Node) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    S >= N,
    L is 2 * N,
    S >= L,
    R is ((2 * N) + 1),
    R > S,
    new_entry(H, L, _, _),
    new_entry(H, L, _, _),
    new_entry(H, N, _, _),
    min(H, N, L, Min),
    N = Min,
    order_heap(H, L),
    !.
order_heap(H, Node) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    S >= N,
    L is 2 * N,
    S >= L,
    R is ((2 * N) + 1),
    R > S,
    new_entry(H, L, _, _),
    new_entry(H, N, _, _),
    min(H, N, L, Min),
    N \= Min,
    new_entry(H, Min, Km, Vm),
    new_entry(H, N, Kn, Vn),
    change(H, Min, Km, Vm, N, Kn, Vn),
    order_heap(H, Min),
    !.
order_heap(H, Node) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    S >= N,
    L is 2 * N,
    S >= L,
    R is ((2 * N) + 1),
    R > S,
    new_entry(H, L, _, _),
    new_entry(H, R, _, _),
    new_entry(H, N, _, _),
    min(H, N, L, R, Min),
    N \= Min,
    new_entry(H, Min, Km, Vm),
    new_entry(H, N, Kn, Vn),
    min(H, N, L, R, Min),
    N = Min,
    !.
order_heap(H, Node) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    S >= N,
    L is 2 * N,
    S >= L,
    R is ((2 * N) + 1),
    R >= S,
    new_entry(H, L, _, _),
    new_entry(H, R, _, _),
    new_entry(H, N, _, _),
    min(H, N, L, R, Min),
    N \= Min,
    new_entry(H, Min, Km, Vm),
    new_entry(H, N, Kn, Vn),
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
propertyHeap(H, _) :-
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
    new_entry(H, N, _, _),
    F is floor(N / 2),
    new_entry(H, F, _, _),
    min(H, N, F, M),
    M = F,
    !.
propertyHeap(H, N) :-
    nonvar(H),
    heap(H, S),
    not_empty(H),
    S \= 1,
    N \= 1,
    new_entry(H, N, Knode, Vnode),
    F is floor(N / 2),
    new_entry(H, F, Kf, Vf),
    min(H, N, F, M),
    M \= F,
    change(H, N, Kn, Vn, F, Kf, Vf),
    propertyHeap(H, F)
    !.

 

%%% end of file -- sssp.pl --
