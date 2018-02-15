#include <stddef.h>

typedef struct matrix
{ int     ndim;
  size_t  esize;				/* size of elements */
  size_t  dim[0];
} matrix;

#define SIZEOF_MHEADER(ndim) \
	((size_t)(&((matrix*)NULL)->dim[ndim]))

size_t
matrix_size_1(size_t esize, size_t dim0)
{ return SIZEOF_MHEADER(1) + esize*dim0;
}

size_t
matrix_size_2(size_t esize, size_t dim0, size_t dim1)
{ return SIZEOF_MHEADER(1) + esize*dim0*dim1;
}

void
matrix_init_1(matrix *m, size_t esize, size_t dim0)
{ m->ndim   = 1;
  m->dim[0] = dim0;
  m->esize  = esize;
}

void
matrix_init_2(matrix *m, size_t esize, size_t dim0, size_t dim1)
{ m->ndim   = 2;
  m->dim[0] = dim0;
  m->dim[1] = dim1;
  m->esize  = esize;
}

size_t
matrix_offset_1(const matrix *m, size_t i0)
{ return SIZEOF_MHEADER(1) + m->esize * i0;
}

size_t
matrix_offset_2(const matrix *m, size_t i0, size_t i1)
{ return SIZEOF_MHEADER(1) + m->esize * (i0*m->dim[0] + i1);
}
