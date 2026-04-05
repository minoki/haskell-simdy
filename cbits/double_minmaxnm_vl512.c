#if defined(__AVX512DQ__)
#include <stdint.h>
#include <immintrin.h>

#pragma STDC FENV_ACCESS ON

__m512d hs_simdy_minimumNumberDoubleX8(__m512d xx, __m512d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m512d one = _mm512_set1_pd(1.0);
    asm volatile("vmulpd %g1, %g0, %g0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %g1, %g0, %g0" : "+v"(yy) : "v"(one));
    return _mm512_range_pd(xx, yy, 4); // NaN is missing data
}

__m512d hs_simdy_maximumNumberDoubleX8(__m512d xx, __m512d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m512d one = _mm512_set1_pd(1.0);
    asm volatile("vmulpd %g1, %g0, %g0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %g1, %g0, %g0" : "+v"(yy) : "v"(one));
    return _mm512_range_pd(xx, yy, 5); // NaN is missing data
}

#endif
