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

// IEEE 754-2019 minimumNumber/maximumNumber:
//   * -0 < +0
//   * NaN is missing data
//   * raise INVALID on signaling NaN

#pragma STDC FENV_ACCESS ON

// Convert (possible) signaling NaN to quiet one
#if defined(__SSE2__)
__attribute__((always_inline)) static inline
double canonicalize_double(double x)
{
    asm volatile("mulsd %1, %0" : "+x"(x) : "x"(1.0));
    return x;
}
#elif defined(__aarch64__)
__attribute__((always_inline)) static inline
double canonicalize_double(double x)
{
    double result;
    asm volatile("fmul %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(1.0));
    return result;
}
#else
__attribute__((always_inline)) static inline
double canonicalize_double(double x)
{
    volatile double one = 1.0;
    return x * one;
}
#endif

double hs_simdy_minimumNumber_double(double x, double y)
{
#if defined(__aarch64__)
    x = canonicalize_double(x);
    y = canonicalize_double(y);
    float64x1_t xx = vld1_f64(&x);
    float64x1_t yy = vld1_f64(&y);
    float64x1_t zz = vminnm_f64(xx, yy);
    double result;
    vst1_f64(&result, zz);
    return result;
    /*
    double result;
    asm("fminnm %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
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
        x = canonicalize_double(x);
        y = canonicalize_double(y);
        return isnan(x) ? y : x; // discard NaN
    } else {
        // equal or both zero
        return signbit(x) ? x : y;
    }
#endif
}

double hs_simdy_maximumNumber_double(double x, double y)
{
#if defined(__aarch64__)
    x = canonicalize_double(x);
    y = canonicalize_double(y);
    float64x1_t xx = vld1_f64(&x);
    float64x1_t yy = vld1_f64(&y);
    float64x1_t zz = vmaxnm_f64(xx, yy);
    double result;
    vst1_f64(&result, zz);
    return result;
    /*
    double result;
    asm("fmaxnm %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
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
        x = canonicalize_double(x);
        y = canonicalize_double(y);
        return isnan(x) ? y : x; // discard NaN
    } else {
        // equal or both zero
        return signbit(x) ? y : x;
    }
#endif
}

#if defined(__i386__) || defined(__x86_64__)

__m128d hs_simdy_minimumNumber_doublex2(__m128d x, __m128d y)
{
    // Convert (possible) signaling NaN to quiet one
    __m128d one = _mm_set1_pd(1.0);
    asm volatile("mulpd %x1, %x0" : "+x"(x) : "x"(one));
    asm volatile("mulpd %x1, %x0" : "+x"(y) : "x"(one));
    __m128d ord = _mm_cmpord_pd(x, y); // non-signaling compare
    __m128d x_ord = _mm_and_pd(x, ord); // convert possible NaN to zero (avoid undue INVALID)
    __m128d y_ord = _mm_and_pd(y, ord); // convert possible NaN to zero (avoid undue INVALID)
    __m128d result_ltgt = _mm_min_pd(x_ord, y_ord); // x_ord < y_ord ? x_ord : y_ord
    __m128d eq = _mm_cmpeq_pd(x, y); // non-signaling compare
    __m128d result_eq = _mm_and_pd(eq, x);
    __m128d result_ord = _mm_or_pd(result_eq, result_ltgt); // ordered
    __m128d x_nan_mask = _mm_cmpunord_pd(x, x);
    __m128d result_unord = _mm_or_pd(_mm_and_pd(x_nan_mask, y), _mm_andnot_pd(x_nan_mask, x)); // isnan(x) ? y : x
    return _mm_or_pd(result_ord, _mm_andnot_pd(ord, result_unord));
}

__m128d hs_simdy_maximumNumber_doublex2(__m128d x, __m128d y)
{
    // Convert (possible) signaling NaN to quiet one
    __m128d one = _mm_set1_pd(1.0);
    asm volatile("mulpd %x1, %x0" : "+x"(x) : "x"(one));
    asm volatile("mulpd %x1, %x0" : "+x"(y) : "x"(one));
    __m128d ord = _mm_cmpord_pd(x, y); // non-signaling compare
    __m128d x_ord = _mm_and_pd(x, ord); // convert possible NaN to zero
    __m128d y_ord = _mm_and_pd(y, ord); // convert possible NaN to zero
    __m128d result_ltgt = _mm_max_pd(x_ord, y_ord); // x_ord > y_ord ? x_ord : y_ord
    __m128d neq = _mm_cmpneq_pd(x, y); // non-signaling (LT || GT || UNORD)
    __m128d result_ord = _mm_and_pd(_mm_or_pd(neq, x), result_ltgt);
    __m128d x_nan_mask = _mm_cmpunord_pd(x, x);
    __m128d result_unord = _mm_or_pd(_mm_and_pd(x_nan_mask, y), _mm_andnot_pd(x_nan_mask, x)); // isnan(x) ? y : x
    return _mm_or_pd(result_ord, _mm_andnot_pd(ord, result_unord));
}

#if defined(__AVX__)
__m256d hs_simdy_minimumNumber_doublex4(__m256d xx, __m256d yy)
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

__m256d hs_simdy_maximumNumber_doublex4(__m256d xx, __m256d yy)
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
#endif

#if defined(__AVX512DQ__) && defined(__AVX512VL__)
__m128d hs_simdy_minimumNumber_doublex2_avx512(__m128d xx, __m128d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m128d one = _mm_set1_pd(1.0);
    asm volatile("vmulpd %x1, %x0, %x0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %x1, %x0, %x0" : "+v"(yy) : "v"(one));
    return _mm_range_pd(xx, yy, 4); // NaN is missing data
}

__m128d hs_simdy_maximumNumber_doublex2_avx512(__m128d xx, __m128d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m128d one = _mm_set1_pd(1.0);
    asm volatile("vmulpd %x1, %x0, %x0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %x1, %x0, %x0" : "+v"(yy) : "v"(one));
    return _mm_range_pd(xx, yy, 5); // NaN is missing data
}

__m256d hs_simdy_minimumNumber_doublex4_avx512(__m256d xx, __m256d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m256d one = _mm256_set1_pd(1.0);
    asm volatile("vmulpd %t1, %t0, %t0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %t1, %t0, %t0" : "+v"(yy) : "v"(one));
    return _mm256_range_pd(xx, yy, 4); // NaN is missing data
}

__m256d hs_simdy_maximumNumber_doublex4_avx512(__m256d xx, __m256d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m256d one = _mm256_set1_pd(1.0);
    asm volatile("vmulpd %t1, %t0, %t0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %t1, %t0, %t0" : "+v"(yy) : "v"(one));
    return _mm256_range_pd(xx, yy, 5); // NaN is missing data
}
#endif

#if defined(__AVX512DQ__)
__m512d hs_simdy_minimumNumber_doublex8(__m512d xx, __m512d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m512d one = _mm512_set1_pd(1.0);
    asm volatile("vmulpd %g1, %g0, %g0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %g1, %g0, %g0" : "+v"(yy) : "v"(one));
    return _mm512_range_pd(xx, yy, 4); // NaN is missing data
}

__m512d hs_simdy_maximumNumber_doublex8(__m512d xx, __m512d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m512d one = _mm512_set1_pd(1.0);
    asm volatile("vmulpd %g1, %g0, %g0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %g1, %g0, %g0" : "+v"(yy) : "v"(one));
    return _mm512_range_pd(xx, yy, 5); // NaN is missing data
}
#endif

#elif defined(__aarch64__)

float64x2_t hs_simdy_minimumNumber_doublex2(float64x2_t x, float64x2_t y)
{
    // Convert (possible) signaling NaN to quiet one
    float64x2_t one = vdupq_n_f64(1.0);
    asm volatile("fmul %0.2d, %0.2d, %1.2d" : "+w"(x) : "w"(one));
    asm volatile("fmul %0.2d, %0.2d, %1.2d" : "+w"(y) : "w"(one));
    // x = vmulq_f32(x, one);
    // y = vmulq_f32(y, one);
    return vminnmq_f64(x, y);
}

float64x2_t hs_simdy_maximumNumber_doublex2(float64x2_t x, float64x2_t y)
{
    // Convert (possible) signaling NaN to quiet one
    float64x2_t one = vdupq_n_f64(1.0);
    asm volatile("fmul %0.2d, %0.2d, %1.2d" : "+w"(x) : "w"(one));
    asm volatile("fmul %0.2d, %0.2d, %1.2d" : "+w"(y) : "w"(one));
    // x = vmulq_f32(x, one);
    // y = vmulq_f32(y, one);
    return vmaxnmq_f64(x, y);
}

#endif
