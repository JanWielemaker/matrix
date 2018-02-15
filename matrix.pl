:- module(matrix,
          [ matrix_new/3,                       % +Type, +Dim, -Matrix

            matrix_type/2,                      % +Matrix, -Type
            matrix_size/2,                      % +Matrix, -NElems

            matrix_set/3,                       % +Matrix, +Position, +Elem
            matrix_get/3,                       % +Matrix, +Position, -Elem

            matrix_set_all/2                    % +Matrix, +Value
          ]).
:- use_module(library(ffi)).

/** <module> Matrix manipulation

@see https://www.dcc.fc.up.pt/~vsc/Yap/documentation.html#matrix
*/

c_define(mptr, *(struct(matrix))).

cpp_const('T_INTS').
cpp_const('T_FLOATS').

:- c_import("#include \"matrix.c\"",
            [ matrix ],
            [ matrix_size_1(int, int, [int]),
              matrix_size_2(int, int, int, [int]),

              matrix_init_1(mptr, int, int),
              matrix_init_2(mptr, int, int, int),

              matrix_type(mptr, [int]) as matrix_typeid,
              matrix_size(mptr, [int]),

              matrix_offset_1(mptr, -int, int, [int]),
              matrix_offset_2(mptr, -int, int, int, [int]),

              matrix_set_all_ints(mptr, int)
            ]).

%!  matrix_new(+Type, +Dim, -Matrix) is det.

matrix_new(Type, [Dim0], Matrix) :-
    !,
    typeid(Type, TypeID),
    matrix_size_1(TypeID, Dim0, Size),
    c_calloc(Matrix, struct(matrix), Size, 1),
    matrix_init_1(Matrix, TypeID, Dim0).
matrix_new(Type, [Dim0, Dim1], Matrix) :-
    typeid(Type, TypeID),
    matrix_size_2(TypeID, Dim0, Dim1, Size),
    c_calloc(Matrix, struct(matrix), Size, 1),
    matrix_init_2(Matrix, TypeID, Dim0, Dim1).

%!  matrix_type(+Matrix, -Type) is det.

matrix_type(Matrix, Type) :-
    matrix_typeid(Matrix, TypeID),
    typeid(Type, TypeID),
    !.

%!  matrix_size(+Matrix, -NElems) is det.
%
%   True when NElems is the number of elements in Matrix.

%!  matrix_set(+Matrix, +Position, +Elem) is det.

matrix_set(Matrix, [Dim0], Elem) :-
    !,
    matrix_offset_1(Matrix, TypeID, Dim0, Offset),
    ctype(TypeID, CType),
    c_store(Matrix, Offset, CType, Elem).
matrix_set(Matrix, [Dim0,Dim1], Elem) :-
    matrix_offset_2(Matrix, TypeID, Dim0, Dim1, Offset),
    ctype(TypeID, CType),
    c_store(Matrix, Offset, CType, Elem).

%!  matrix_get(+Matrix, +Position, -Elem) is det.

matrix_get(Matrix, [Dim0], Elem) :-
    !,
    matrix_offset_1(Matrix, TypeID, Dim0, Offset),
    ctype(TypeID, CType),
    c_load(Matrix, Offset, CType, Elem).
matrix_get(Matrix, [Dim0,Dim1], Elem) :-
    matrix_offset_2(Matrix, TypeID, Dim0, Dim1, Offset),
    ctype(TypeID, CType),
    c_load(Matrix, Offset, CType, Elem).

%!  matrix_set_all(+Matrix, +Elem) is det.
%
%   Set all elements of Matrix to Elem

matrix_set_all(Matrix, Elem) :-
    matrix_typeid(Matrix, TypeID),
    matrix_set_all(TypeID, Matrix, Elem).

matrix_set_all('T_INTS',   Matrix, Elem) :- matrix_set_all_ints(Matrix, Elem).
matrix_set_all('T_FLOATS', Matrix, Elem) :- matrix_set_all_floats(Matrix, Elem).

typeid(ints,   'T_INTS').
typeid(floats, 'T_FLOATS').

ctype('T_INTS',   longlong).
ctype('T_FLOATS', double).
