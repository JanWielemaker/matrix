#include <stddef.h>
#include <assert.h>

typedef long long m_int;
typedef double    m_float;

typedef enum mtype
{ ints,
  floats
} mtype;

typedef struct matrix
{ int     ndim;
  mtype   type;				/* Type of elements */
  size_t  dim[0];
} matrix;

#define SIZEOF_MHEADER(ndim) \
	((size_t)(&((matrix*)NULL)->dim[ndim]))

static inline size_t
esize(mtype type)
{ switch(type)
  { case ints:   return sizeof(m_int);
    case floats: return sizeof(m_float);
    default:	 assert(0);
  }
}

size_t
matrix_size_1(mtype type, size_t dim0)
{ return SIZEOF_MHEADER(1) + esize(type)*dim0;
}

size_t
matrix_size_2(mtype type, size_t dim0, size_t dim1)
{ return SIZEOF_MHEADER(1) + esize(type)*dim0*dim1;
}

void
matrix_init_1(matrix *m, mtype type, size_t dim0)
{ m->ndim   = 1;
  m->type   = type;
  m->dim[0] = dim0;
}

void
matrix_init_2(matrix *m, mtype type, size_t dim0, size_t dim1)
{ m->ndim   = 2;
  m->type   = type;
  m->dim[0] = dim0;
  m->dim[1] = dim1;
}

size_t
matrix_offset_1(const matrix *m, size_t i0)
{ return SIZEOF_MHEADER(1) + esize(m->type) * i0;
}

size_t
matrix_offset_2(const matrix *m, size_t i0, size_t i1)
{ return SIZEOF_MHEADER(1) + esize(m->type) * (i0*m->dim[0] + i1);
}

mtype
matrix_type(const matrix *m)
{ return m->type;
}
