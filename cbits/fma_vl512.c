#if defined(__AVX512F__)
#include <immintrin.h>

__m512 hs_simdy_fmaddFloatX16(__m512 x, __m512 y, __m512 z)
{
    return _mm512_fmadd_ps(x, y, z);
}

__m512d hs_simdy_fmaddDoubleX8(__m512d x, __m512d y, __m512d z)
{
    return _mm512_fmadd_pd(x, y, z);
}

#endif
