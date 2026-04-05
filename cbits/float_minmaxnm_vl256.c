#if defined(__AVX__)
#include <stdint.h>
#include <immintrin.h>

__m256 hs_simdy_minimumNumberFloatX8(__m256 xx, __m256 yy)
{
    __m256 unord = _mm256_cmp_ps(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_ps(unord, unord)) {
        // Convert (possible) signaling NaN to quiet one
        __m256 one = _mm256_set1_ps(1.0f);
        asm volatile("vmulps %t1, %t0, %t0" : "+x"(xx) : "x"(one));
        asm volatile("vmulps %t1, %t0, %t0" : "+x"(yy) : "x"(one));
        __m256 x_ord = _mm256_andnot_ps(unord, xx); // convert possible NaN to zero (avoid undue INVALID)
        __m256 y_ord = _mm256_andnot_ps(unord, yy); // convert possible NaN to zero (avoid undue INVALID)
        __m256 result_ltgt = _mm256_min_ps(x_ord, y_ord); // x_ord < y_ord ? x_ord : y_ord
        __m256 eq = _mm256_cmp_ps(xx, yy, 0); // non-signaling EQ compare
        __m256 result_eq = _mm256_and_ps(eq, xx);
        __m256 result_ord = _mm256_or_ps(result_eq, result_ltgt); // ordered
        __m256 x_nan_mask = _mm256_cmp_ps(xx, xx, 3); // non-signaling UNORD
        __m256 result_unord = _mm256_blendv_ps(xx, yy, x_nan_mask); // isnan(x) ? y : x
        return _mm256_blendv_ps(result_ord, result_unord, unord);
    } else {
        // No NaN in input
        __m256 neq = _mm256_cmp_ps(xx, yy, 0xc); // not-equal, non-signaling
        __m256 zz_neq = _mm256_min_ps(xx, yy);
        __m256 zz_eq = _mm256_or_ps(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        return _mm256_blendv_ps(zz_eq, zz_neq, neq);
    }
}

__m256 hs_simdy_maximumNumberFloatX8(__m256 xx, __m256 yy)
{
    __m256 unord = _mm256_cmp_ps(xx, yy, 3); // non-signaling UNORD compare
    if (!_mm256_testz_ps(unord, unord)) {
        // Convert (possible) signaling NaN to quiet one
        __m256 one = _mm256_set1_ps(1.0f);
        asm volatile("vmulps %t1, %t0, %t0" : "+x"(xx) : "x"(one));
        asm volatile("vmulps %t1, %t0, %t0" : "+x"(yy) : "x"(one));
        __m256 x_ord = _mm256_andnot_ps(unord, xx); // convert possible NaN to zero
        __m256 y_ord = _mm256_andnot_ps(unord, yy); // convert possible NaN to zero
        __m256 result_ltgt = _mm256_max_ps(x_ord, y_ord); // x_ord > y_ord ? x_ord : y_ord
        __m256 neq = _mm256_cmp_ps(xx, yy, 4); // non-signaling (LT || GT || UNORD)
        __m256 result_ord = _mm256_and_ps(_mm256_or_ps(neq, xx), result_ltgt);
        __m256 x_nan_mask = _mm256_cmp_ps(xx, xx, 3); // non-signaling UNORD
        __m256 result_unord = _mm256_blendv_ps(xx, yy, x_nan_mask); // isnan(x) ? y : x
        return _mm256_blendv_ps(result_ord, result_unord, unord);
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
__m256 hs_simdy_minimumNumberFloatX8_avx512(__m256 xx, __m256 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m256 one = _mm256_set1_ps(1.0f);
    asm volatile("vmulps %t1, %t0, %t0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %t1, %t0, %t0" : "+v"(yy) : "v"(one));
    return _mm256_range_ps(xx, yy, 4); // NaN is missing data
}

__attribute__((target("avx512vl,avx512dq")))
__m256 hs_simdy_maximumNumberFloatX8_avx512(__m256 xx, __m256 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m256 one = _mm256_set1_ps(1.0f);
    asm volatile("vmulps %t1, %t0, %t0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %t1, %t0, %t0" : "+v"(yy) : "v"(one));
    return _mm256_range_ps(xx, yy, 5); // NaN is missing data
}

#endif
