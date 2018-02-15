# A prototype SWI-Prolog matrix library

This repo illustrates a possible way to use the `ffi` package to realise
a [YAP-style matrix
library](https://www.dcc.fc.up.pt/~vsc/Yap/documentation.html#matrix).

To use it,

  - install the experimental `ffi` package from
    https://github.com/JanWielemaker/ffi
  - Compile matrix.c using

    ```
    gcc -shared -fPIC -o matrix.so matrix.c
    ```

Run

```
swipl matrix.pl
?- matrix_new(ints, [1000, 1000], M).
M = <C struct matrix[1]>(0x7f5834d0d010).
```

## Using a matrix library

The code just defines our own matrix structure. We should select a
matrix library and make the basic interface such that it is easy to
access functions from that library to manage our matrices. See e.g.,
https://stackoverflow.com/questions/4501322/c-libraries-for-mathematical-matrix-operations

## Background

This was initiated by Fabrizio Riguzzi and Marco Alberti based on YAP
Prolog's matrix library.
