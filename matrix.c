#include <stddef.h>
#include <assert.h>

typedef long long m_int;
typedef double    m_float;

#define T_INTS   0
#define T_FLOATS 1

typedef int mtype;

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
  { case T_INTS:   return sizeof(m_int);
    case T_FLOATS: return sizeof(m_float);
    default:	   assert(0);
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
matrix_offset_1(const matrix *m, mtype *tp, size_t i0)
{ *tp = m->type;
  return SIZEOF_MHEADER(1) + esize(m->type) * i0;
}

size_t
matrix_offset_2(const matrix *m, mtype *tp, size_t i0, size_t i1)
{ *tp = m->type;
  return SIZEOF_MHEADER(1) + esize(m->type) * (i0*m->dim[0] + i1);
}

mtype
matrix_type(const matrix *m)
{ return m->type;
}

size_t
matrix_size(const matrix *m)
{ if ( m->ndim > 0 )
  { size_t s = m->dim[0];

    for(size_t i=1; i<m->ndim; i++)
      s *= m->dim[i];

    return s;
  }

  return 0;
}

static inline m_int *
matrix_ints(const matrix *m)
{ return (m_int*)((const char*)m + SIZEOF_MHEADER(m->ndim));
}

static inline m_float *
matrix_floats(const matrix *m)
{ return (m_float*)((const char*)m + SIZEOF_MHEADER(m->ndim));
}


void
matrix_set_all_ints(matrix *m, m_int v)
{ m_int *vs = matrix_ints(m);
  size_t size = matrix_size(m);

  for(size_t i=0; i<size; i++)
    vs[i] = v;
}

void
matrix_set_all_floats(matrix *m, m_float v)
{ m_float *vs = matrix_floats(m);
  size_t size = matrix_size(m);

  for(size_t i=0; i<size; i++)
    vs[i] = v;
}

