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
            [ matrix_size_1(enum, int, [int]),
              matrix_size_2(enum, int, int, [int]),

              matrix_init_1(mptr, enum, int),
              matrix_init_2(mptr, enum, int, int),

              matrix_type(mptr, [enum]),

              matrix_offset_1(mptr, int, [int]),
              matrix_offset_2(mptr, int, int, [int])
            ]).

%!  matrix_new(+Type, +Dim, -Matrix) is det.

matrix_new(Type, [Dim0], Matrix) :-
    !,
    matrix_size_1(Type, Dim0, Size),
    c_calloc(Matrix, struct(matrix), Size, 1),
    matrix_init_1(Matrix, Type, Dim0).
matrix_new(Type, [Dim0, Dim1], Matrix) :-
    matrix_size_2(Type, Dim0, Dim1, Size),
    c_calloc(Matrix, struct(matrix), Size, 1),
    matrix_init_2(Matrix, Type, Dim0, Dim1).

%!  matrix_set(+Matrix, +Position, +Elem) is det.

matrix_set(Matrix, [Dim0], Elem) :-
    !,
    matrix_offset_1(Matrix, Dim0, Offset),
    matrix_ctype(Matrix, CType),
    c_store(Matrix, Offset, CType, Elem).
matrix_set(Matrix, [Dim0,Dim1], Elem) :-
    matrix_offset_2(Matrix, Dim0, Dim1, Offset),
    matrix_ctype(Matrix, CType),
    c_store(Matrix, Offset, CType, Elem).

%!  matrix_get(+Matrix, +Position, -Elem) is det.

matrix_get(Matrix, [Dim0], Elem) :-
    !,
    matrix_offset_1(Matrix, Dim0, Offset),
    matrix_ctype(Matrix, CType),
    c_load(Matrix, Offset, CType, Elem).
matrix_get(Matrix, [Dim0,Dim1], Elem) :-
    matrix_offset_2(Matrix, Dim0, Dim1, Offset),
    matrix_ctype(Matrix, CType),
    c_load(Matrix, Offset, CType, Elem).

matrix_ctype(Matrix, Type) :-
    matrix_type(Matrix, Type0),
    c_type(Type0, Type).

c_type(ints, longlong).
c_type(floats, double).
