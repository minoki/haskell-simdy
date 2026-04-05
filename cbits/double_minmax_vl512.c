#if defined(__AVX512DQ__)
#include <stdint.h>
#include <immintrin.h>

#pragma STDC FENV_ACCESS ON

__m512d hs_simdy_minimumDoubleX8(__m512d xx, __m512d yy)
{
    __m512d m = _mm512_range_pd(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm512_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm512_mask_add_pd
    __m512d result = m;
    asm("vaddpd %g3, %g2, %g0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm512_mask_add_pd(m, unord, xx, yy); // propagate NaN
#endif
}

__m512d hs_simdy_maximumDoubleX8(__m512d xx, __m512d yy)
{
    __m512d m = _mm512_range_pd(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm512_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm512_mask_add_pd
    __m512d result = m;
    asm("vaddpd %g3, %g2, %g0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm512_mask_add_pd(m, unord, xx, yy); // propagate NaN
#endif
}

#endif
