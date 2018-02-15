:- module(matrix,
          [ matrix_new/3,                       % +Type, +Dim, -Matrix
            matrix_set/3,                       % +Matrix, +Position, +Elem
            matrix_get/3                        % +Matrix, +Position, -Elem
          ]).
:- use_module(library(ffi)).

/** <module> Matrix manipulation

@see https://www.dcc.fc.up.pt/~vsc/Yap/documentation.html#matrix
*/

c_define(mptr, *(struct(matrix))).

:- c_import("#include \"matrix.c\"",
            [ matrix ],
            [ matrix_size_1(int, int, [int]),
              matrix_size_2(int, int, int, [int]),

              matrix_init_1(mptr, int, int),
              matrix_init_2(mptr, int, int, int),

              matrix_offset_1(mptr, int, [int]),
              matrix_offset_2(mptr, int, int, [int])
            ]).

%!  matrix_new(+Type, +Dim, -Matrix) is det.

matrix_new(ints, [Dim0], Matrix) :-
    !,
    c_sizeof(int, Esize),
    matrix_size_1(Esize, Dim0, Size),
    c_calloc(Matrix, struct(matrix), Size, 1),
    matrix_init_1(Matrix, Esize, Dim0).
matrix_new(ints, [Dim0, Dim1], Matrix) :-
    c_sizeof(int, Esize),
    matrix_size_2(Esize, Dim0, Dim1, Size),
    c_calloc(Matrix, struct(matrix), Size, 1),
    matrix_init_2(Matrix, Esize, Dim0, Dim1).

%!  matrix_set(+Matrix, +Position, +Elem) is det.

matrix_set(Matrix, [Dim0], Elem) :-
    !,
    matrix_offset_1(Matrix, Dim0, Offset),
    c_store(Matrix, Offset, int, Elem).
matrix_set(Matrix, [Dim0,Dim1], Elem) :-
    matrix_offset_2(Matrix, Dim0, Dim1, Offset),
    c_store(Matrix, Offset, int, Elem).

%!  matrix_get(+Matrix, +Position, -Elem) is det.

matrix_get(Matrix, [Dim0], Elem) :-
    !,
    matrix_offset_1(Matrix, Dim0, Offset),
    c_load(Matrix, Offset, int, Elem).
matrix_get(Matrix, [Dim0,Dim1], Elem) :-
    matrix_offset_2(Matrix, Dim0, Dim1, Offset),
    c_load(Matrix, Offset, int, Elem).

