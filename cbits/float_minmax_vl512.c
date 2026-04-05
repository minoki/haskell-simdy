#if defined(__AVX512DQ__)
#include <stdint.h>
#include <immintrin.h>

#pragma STDC FENV_ACCESS ON

__m512 hs_simdy_minimumFloatX16(__m512 xx, __m512 yy)
{
    __m512 m = _mm512_range_ps(xx, yy, 4); // NaN is missing data
    __mmask16 unord = _mm512_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm512_mask_add_ps
    __m512 result = m;
    asm("vaddps %g3, %g2, %g0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm512_mask_add_ps(m, unord, xx, yy); // propagate NaN
#endif
}

__m512 hs_simdy_maximumFloatX16(__m512 xx, __m512 yy)
{
    __m512 m = _mm512_range_ps(xx, yy, 5); // NaN is missing data
    __mmask16 unord = _mm512_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm512_mask_add_ps
    __m512 result = m;
    asm("vaddps %g3, %g2, %g0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm512_mask_add_ps(m, unord, xx, yy); // propagate NaN
#endif
}

#endif
