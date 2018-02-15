:- use_module(library(statistics)).
:- use_module(library(apply_macros), []).
:- use_module(matrix).

b_create(N, Type, Dim) :-
    time(forall(between(1, N, _),
                matrix_new(Type, Dim, _))).

%!  b_fill(+Type, +Dim, +Value) is det.
%
%   Create a matrix for elements of Type   with the given dimensions and
%   fill it. Benchmarks setting individual values. For example:
%
%   ```
%   ?- time(b_fill(ints, [1000,1000], 42)).
%   % 5,001,260 inferences, 0.557 CPU in 0.558 seconds (100% CPU, 8981082 Lips)
%   ```

b_fill(Type, Dim, Value) :-
    matrix_new(Type, Dim, M),
    fill(M, Dim, Value).

fill(M, [Dim0], Value) :-
    Max is Dim0-1,
    forall(between(0, Max, I),
           matrix_set(M, [I], Value)).
fill(M, [Dim0,Dim1], Value) :-
    Max0 is Dim0-1,
    Max1 is Dim1-1,
    forall(between(0, Max0, D0),
           forall(between(0, Max1, D1),
                  matrix_set(M, [D0,D1], Value))).
