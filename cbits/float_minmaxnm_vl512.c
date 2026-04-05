#if defined(__AVX512DQ__)
#include <stdint.h>
#include <immintrin.h>

__m512 hs_simdy_minimumNumberFloatX16(__m512 xx, __m512 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m512 one = _mm512_set1_ps(1.0f);
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(yy) : "v"(one));
    return _mm512_range_ps(xx, yy, 4); // NaN is missing data
}

__m512 hs_simdy_maximumNumberFloatX16(__m512 xx, __m512 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m512 one = _mm512_set1_ps(1.0f);
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(yy) : "v"(one));
    return _mm512_range_ps(xx, yy, 5); // NaN is missing data
}

#endif
