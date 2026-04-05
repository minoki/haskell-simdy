#if defined(__AVX__)
#include <stdint.h>
#include <immintrin.h>

#pragma STDC FENV_ACCESS ON

__m256d hs_simdy_minimumNumberDoubleX4(__m256d xx, __m256d yy)
{
    __m256d unord = _mm256_cmp_pd(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_pd(unord, unord)) {
        // Convert (possible) signaling NaN to quiet one
        __m256d one = _mm256_set1_pd(1.0);
        asm volatile("vmulpd %t1, %t0, %t0" : "+x"(xx) : "x"(one));
        asm volatile("vmulpd %t1, %t0, %t0" : "+x"(yy) : "x"(one));
        __m256d x_ord = _mm256_andnot_pd(unord, xx); // convert possible NaN to zero (avoid undue INVALID)
        __m256d y_ord = _mm256_andnot_pd(unord, yy); // convert possible NaN to zero (avoid undue INVALID)
        __m256d result_ltgt = _mm256_min_pd(x_ord, y_ord); // x_ord < y_ord ? x_ord : y_ord
        __m256d eq = _mm256_cmp_pd(xx, yy, 0); // non-signaling EQ compare
        __m256d result_eq = _mm256_and_pd(eq, xx);
        __m256d result_ord = _mm256_or_pd(result_eq, result_ltgt); // ordered
        __m256d x_nan_mask = _mm256_cmp_pd(xx, xx, 3); // non-signaling UNORD
        __m256d result_unord = _mm256_blendv_pd(xx, yy, x_nan_mask); // isnan(x) ? y : x
        return _mm256_blendv_pd(result_ord, result_unord, unord);
    } else {
        // No NaN in input
        __m256d neq = _mm256_cmp_pd(xx, yy, 0x4); // not-equal, non-signaling
        __m256d zz_neq = _mm256_min_pd(xx, yy);
        __m256d zz_eq = _mm256_or_pd(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        return _mm256_blendv_pd(zz_eq, zz_neq, neq);
    }
}

__m256d hs_simdy_maximumNumberDoubleX4(__m256d xx, __m256d yy)
{
    __m256d unord = _mm256_cmp_pd(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_pd(unord, unord)) {
        // Convert (possible) signaling NaN to quiet one
        __m256d one = _mm256_set1_pd(1.0);
        asm volatile("vmulpd %t1, %t0, %t0" : "+x"(xx) : "x"(one));
        asm volatile("vmulpd %t1, %t0, %t0" : "+x"(yy) : "x"(one));
        __m256d x_ord = _mm256_andnot_pd(unord, xx); // convert possible NaN to zero
        __m256d y_ord = _mm256_andnot_pd(unord, yy); // convert possible NaN to zero
        __m256d result_ltgt = _mm256_max_pd(x_ord, y_ord); // x_ord > y_ord ? x_ord : y_ord
        __m256d neq = _mm256_cmp_pd(xx, yy, 4); // non-signaling (LT || GT || UNORD)
        __m256d result_ord = _mm256_and_pd(_mm256_or_pd(neq, xx), result_ltgt);
        __m256d x_nan_mask = _mm256_cmp_pd(xx, xx, 3); // non-signaling UNORD
        __m256d result_unord = _mm256_blendv_pd(xx, yy, x_nan_mask); // isnan(x) ? y : x
        return _mm256_blendv_pd(result_ord, result_unord, unord);
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
__m256d hs_simdy_minimumNumberDoubleX4_avx512(__m256d xx, __m256d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m256d one = _mm256_set1_pd(1.0);
    asm volatile("vmulpd %t1, %t0, %t0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %t1, %t0, %t0" : "+v"(yy) : "v"(one));
    return _mm256_range_pd(xx, yy, 4); // NaN is missing data
}

__attribute__((target("avx512vl,avx512dq")))
__m256d hs_simdy_maximumNumberDoubleX4_avx512(__m256d xx, __m256d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m256d one = _mm256_set1_pd(1.0);
    asm volatile("vmulpd %t1, %t0, %t0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %t1, %t0, %t0" : "+v"(yy) : "v"(one));
    return _mm256_range_pd(xx, yy, 5); // NaN is missing data
}

#endif
