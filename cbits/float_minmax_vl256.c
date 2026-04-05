#if defined(__AVX__)
#include <stdint.h>
#include <immintrin.h>

__m256 hs_simdy_minimumFloatX8(__m256 xx, __m256 yy)
{
    __m256 unord = _mm256_cmp_ps(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_ps(unord, unord)) {
        // Some NaN in input
        __m256 x_ord = _mm256_andnot_ps(unord, xx); // convert possible NaN to zero
        __m256 y_ord = _mm256_andnot_ps(unord, yy); // convert possible NaN to zero
        __m256 result_ltgt = _mm256_min_ps(x_ord, y_ord);
        __m256 eq = _mm256_cmp_ps(xx, yy, 0); // non-signaling EQ compare
        __m256 result_eq = _mm256_and_ps(eq, xx);
        __m256 result_ord = _mm256_or_ps(result_eq, result_ltgt); // ordered
        __m256 x_unord = _mm256_and_ps(unord, xx); // ~ord & x
        __m256 y_unord = _mm256_and_ps(unord, yy); // ~ord & y
        __m256 result_unord = _mm256_add_ps(x_unord, y_unord); // propagate NaN
        return _mm256_or_ps(result_ord, result_unord);
    } else {
        // No NaN in input
        __m256 neq = _mm256_cmp_ps(xx, yy, 0xc); // not-equal, non-signaling
        __m256 zz_neq = _mm256_min_ps(xx, yy);
        __m256 zz_eq = _mm256_or_ps(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        return _mm256_blendv_ps(zz_eq, zz_neq, neq);
    }
}

__m256 hs_simdy_maximumFloatX8(__m256 xx, __m256 yy)
{
    __m256 unord = _mm256_cmp_ps(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_ps(unord, unord)) {
        // Some NaN in input
        __m256 x_ord = _mm256_andnot_ps(unord, xx); // convert possible NaN to zero
        __m256 y_ord = _mm256_andnot_ps(unord, yy); // convert possible NaN to zero
        __m256 result_ltgt = _mm256_max_ps(x_ord, y_ord);
        __m256 neq = _mm256_cmp_ps(xx, yy, 4); // non-signaling (LT || GT || UNORD)
        __m256 result_ord = _mm256_and_ps(_mm256_or_ps(neq, xx), result_ltgt);
        __m256 x_unord = _mm256_and_ps(unord, xx); // ~ord & x
        __m256 y_unord = _mm256_and_ps(unord, yy); // ~ord & y
        __m256 result_unord = _mm256_add_ps(x_unord, y_unord); // propagate NaN
        return _mm256_or_ps(result_ord, result_unord);
    } else {
        // No NaN in input
        __m256 neq = _mm256_cmp_ps(xx, yy, 0xc); // not-equal, non-signaling
        __m256 zz_neq = _mm256_max_ps(xx, yy);
        __m256 zz_eq = _mm256_and_ps(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        return _mm256_blendv_ps(zz_eq, zz_neq, neq);
    }
}

__attribute__((target("avx512vl,avx512dq")))
__m256 hs_simdy_minimumFloatX8_avx512(__m256 xx, __m256 yy)
{
    __m256 m = _mm256_range_ps(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm256_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm256_mask_add_ps
    __m256 result = m;
    asm("vaddps %t3, %t2, %t0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm256_mask_add_ps(m, unord, xx, yy); // propagate NaN
#endif
}

__attribute__((target("avx512vl,avx512dq")))
__m256 hs_simdy_maximumFloatX8_avx512(__m256 xx, __m256 yy)
{
    __m256 m = _mm256_range_ps(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm256_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm256_mask_add_ps
    __m256 result = m;
    asm("vaddps %t3, %t2, %t0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm256_mask_add_ps(m, unord, xx, yy); // propagate NaN
#endif
}

#endif
