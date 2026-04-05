#if defined(__AVX__)
#include <stdint.h>
#include <immintrin.h>

__m256d hs_simdy_minimumDoubleX4(__m256d xx, __m256d yy)
{
    __m256d unord = _mm256_cmp_pd(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_pd(unord, unord)) {
        // Some NaN in input
        __m256d x_ord = _mm256_andnot_pd(unord, xx); // convert possible NaN to zero
        __m256d y_ord = _mm256_andnot_pd(unord, yy); // convert possible NaN to zero
        __m256d result_ltgt = _mm256_min_pd(x_ord, y_ord);
        __m256d eq = _mm256_cmp_pd(xx, yy, 0); // non-signaling EQ compare
        __m256d result_eq = _mm256_and_pd(eq, xx);
        __m256d result_ord = _mm256_or_pd(result_eq, result_ltgt); // ordered
        __m256d x_unord = _mm256_and_pd(unord, xx); // ~ord & x
        __m256d y_unord = _mm256_and_pd(unord, yy); // ~ord & y
        __m256d result_unord = _mm256_add_pd(x_unord, y_unord); // propagate NaN
        return _mm256_or_pd(result_ord, result_unord);
    } else {
        // No NaN in input
        __m256d neq = _mm256_cmp_pd(xx, yy, 0x4); // not-equal, non-signaling
        __m256d zz_neq = _mm256_min_pd(xx, yy);
        __m256d zz_eq = _mm256_or_pd(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        return _mm256_blendv_pd(zz_eq, zz_neq, neq);
    }
}

__m256d hs_simdy_maximumDoubleX4(__m256d xx, __m256d yy)
{
    __m256d unord = _mm256_cmp_pd(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_pd(unord, unord)) {
        // Some NaN in input
        __m256d x_ord = _mm256_andnot_pd(unord, xx); // convert possible NaN to zero
        __m256d y_ord = _mm256_andnot_pd(unord, yy); // convert possible NaN to zero
        __m256d result_ltgt = _mm256_max_pd(x_ord, y_ord);
        __m256d neq = _mm256_cmp_pd(xx, yy, 4); // non-signaling (LT || GT || UNORD)
        __m256d result_ord = _mm256_and_pd(_mm256_or_pd(neq, xx), result_ltgt);
        __m256d x_unord = _mm256_and_pd(unord, xx); // ~ord & x
        __m256d y_unord = _mm256_and_pd(unord, yy); // ~ord & y
        __m256d result_unord = _mm256_add_pd(x_unord, y_unord); // propagate NaN
        return _mm256_or_pd(result_ord, result_unord);
    } else {
        // No NaN in input
        __m256d neq = _mm256_cmp_pd(xx, yy, 0x4); // not-equal, non-signaling
        __m256d zz_neq = _mm256_max_pd(xx, yy);
        __m256d zz_eq = _mm256_and_pd(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        return _mm256_blendv_pd(zz_eq, zz_neq, neq);
    }
}

__attribute__((target("avx512vl,avx512dq")))
__m256d hs_simdy_minimumDoubleX4_avx512(__m256d xx, __m256d yy)
{
    __m256d m = _mm256_range_pd(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm256_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm256_mask_add_pd
    __m256d result = m;
    asm("vaddpd %t3, %t2, %t0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm256_mask_add_pd(m, unord, xx, yy); // propagate NaN
#endif
}

__attribute__((target("avx512vl,avx512dq")))
__m256d hs_simdy_maximumDoubleX4_avx512(__m256d xx, __m256d yy)
{
    __m256d m = _mm256_range_pd(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm256_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm256_mask_add_pd
    __m256d result = m;
    asm("vaddpd %t3, %t2, %t0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm256_mask_add_pd(m, unord, xx, yy); // propagate NaN
#endif
}

#endif
