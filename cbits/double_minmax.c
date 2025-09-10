#include <math.h>
#include <stdint.h>
#include <fenv.h>
#include <stdbool.h>
#if defined(__SSE2__)
#include <xmmintrin.h>
#include <immintrin.h>
#elif defined(__aarch64__)
#include <arm_neon.h>
#endif

// IEEE 754-2019 minimum/maximum:
//   * -0 < +0
//   * propagate NaN
//   * raise INVALID on signaling NaN

#pragma STDC FENV_ACCESS ON

double hs_simdy_minimum_double(double x, double y)
{
#if defined(__aarch64__)
    float64x1_t xx = vld1_f64(&x);
    float64x1_t yy = vld1_f64(&y);
    float64x1_t zz = vmin_f64(xx, yy);
    double result;
    vst1_f64(&result, zz);
    return result;
    /*
    double result;
    asm("fmin %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
    return result;
    */
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return x;
    } else if (isgreater(x, y)) {
        return y;
    } else if (isunordered(x, y)) {
        return x + y; // propagate NaN
    } else {
        // equal or both zero
        return signbit(x) ? x : y;
    }
#endif
}

double hs_simdy_maximum_double(double x, double y)
{
#if defined(__aarch64__)
    float64x1_t xx = vld1_f64(&x);
    float64x1_t yy = vld1_f64(&y);
    float64x1_t zz = vmax_f64(xx, yy);
    double result;
    vst1_f64(&result, zz);
    return result;
    /*
    double result;
    asm("fmax %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
    return result;
    */
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return y;
    } else if (isgreater(x, y)) {
        return x;
    } else if (isunordered(x, y)) {
        return x + y; // propagate NaN
    } else {
        // equal or both zero
        return signbit(x) ? y : x;
    }
#endif
}

#if defined(__i386__) || defined(__x86_64__)

__m128d hs_simdy_minimum_doublex2(__m128d x, __m128d y)
{
    __m128d ord = _mm_cmpord_pd(x, y); // non-signaling compare
    __m128d x_ord = _mm_and_pd(x, ord); // convert possible NaN to zero
    __m128d y_ord = _mm_and_pd(y, ord); // convert possible NaN to zero
    __m128d result_ltgt = _mm_min_pd(x_ord, y_ord); // x_ord < y_ord ? x_ord : y_ord
    __m128d eq = _mm_cmpeq_pd(x, y); // non-signaling compare
    __m128d result_eq = _mm_and_pd(eq, x);
    __m128d result_ord = _mm_or_pd(result_eq, result_ltgt); // ordered
    __m128d x_unord = _mm_andnot_pd(ord, x); // ~ord & x
    __m128d y_unord = _mm_andnot_pd(ord, y); // ~ord & y
    __m128d result_unord = _mm_add_pd(x_unord, y_unord); // propagate NaN
    return _mm_or_pd(result_ord, result_unord);
}

__m128d hs_simdy_maximum_doublex2(__m128d x, __m128d y)
{
    __m128d ord = _mm_cmpord_pd(x, y); // non-signaling compare
    __m128d x_ord = _mm_and_pd(x, ord); // convert possible NaN to zero
    __m128d y_ord = _mm_and_pd(y, ord); // convert possible NaN to zero
    __m128d result_ltgt = _mm_max_pd(x_ord, y_ord); // x_ord > y_ord ? x_ord : y_ord
    __m128d neq = _mm_cmpneq_pd(x, y); // non-signaling (LT || GT || UNORD)
    __m128d result_ord = _mm_and_pd(_mm_or_pd(neq, x), result_ltgt);
    __m128d x_unord = _mm_andnot_pd(ord, x); // ~ord & x
    __m128d y_unord = _mm_andnot_pd(ord, y); // ~ord & y
    __m128d result_unord = _mm_add_pd(x_unord, y_unord); // propagate NaN
    return _mm_or_pd(result_ord, result_unord);
}

__attribute__((target("avx")))
__m256d hs_simdy_minimum_doublex4(__m256d xx, __m256d yy)
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
        __m256d neq = _mm256_cmp_pd(xx, yy, 0xc); // not-equal, non-signaling
        __m256d zz_neq = _mm256_min_pd(xx, yy);
        __m256d zz_eq = _mm256_or_pd(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        return _mm256_blendv_pd(zz_eq, zz_neq, neq);
    }
}

__attribute__((target("avx")))
__m256d hs_simdy_maximum_doublex4(__m256d xx, __m256d yy)
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
        __m256d neq = _mm256_cmp_pd(xx, yy, 0xc); // not-equal, non-signaling
        __m256d zz_neq = _mm256_max_pd(xx, yy);
        __m256d zz_eq = _mm256_and_pd(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        return _mm256_blendv_pd(zz_eq, zz_neq, neq);
    }
}

__attribute__((target("avx512dq,avx512vl")))
__m128d hs_simdy_minimum_doublex2_avx512(__m128d xx, __m128d yy)
{
    __m128d m = _mm_range_pd(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm_mask_add_pd(m, unord, xx, yy); // propagate NaN
}

__attribute__((target("avx512dq,avx512vl")))
__m128d hs_simdy_maximum_doublex2_avx512(__m128d xx, __m128d yy)
{
    __m128d m = _mm_range_pd(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm_mask_add_pd(m, unord, xx, yy); // propagate NaN
}

__attribute__((target("avx512dq,avx512vl")))
__m256d hs_simdy_minimum_doublex4_avx512(__m256d xx, __m256d yy)
{
    __m256d m = _mm256_range_pd(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm256_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm256_mask_add_pd(m, unord, xx, yy); // propagate NaN
}

__attribute__((target("avx512dq,avx512vl")))
__m256d hs_simdy_maximum_doublex4_avx512(__m256d xx, __m256d yy)
{
    __m256d m = _mm256_range_pd(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm256_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm256_mask_add_pd(m, unord, xx, yy); // propagate NaN
}

__attribute__((target("avx512dq")))
__m512d hs_simdy_minimum_doublex8(__m512d xx, __m512d yy)
{
    __m512d m = _mm512_range_pd(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm512_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm512_mask_add_pd(m, unord, xx, yy); // propagate NaN
}

__attribute__((target("avx512dq")))
__m512d hs_simdy_maximum_doublex8(__m512d xx, __m512d yy)
{
    __m512d m = _mm512_range_pd(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm512_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm512_mask_add_pd(m, unord, xx, yy); // propagate NaN
}

#elif defined(__aarch64__)

float64x2_t hs_simdy_minimum_doublex2(float64x2_t x, float64x2_t y)
{
    return vminq_f64(x, y);
}

float64x2_t hs_simdy_maximum_doublex2(float64x2_t x, float64x2_t y)
{
    return vmaxq_f64(x, y);
}

#endif
