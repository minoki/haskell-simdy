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

// Convert (possible) signaling NaN to quiet one
#if defined(__SSE2__)
__attribute__((always_inline)) static inline
float canonicalize_float(float x)
{
    asm volatile("mulss %1, %0" : "+x"(x) : "x"(1.0f));
    return x;
}
#elif defined(__aarch64__)
__attribute__((always_inline)) static inline
float canonicalize_float(float x)
{
    float result;
    asm volatile("fmul %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(1.0f));
    return result;
}
#else
__attribute__((always_inline)) static inline
float canonicalize_float(float x)
{
    volatile float one = 1.0f;
    return x * one;
}
#endif

float hs_simdy_minimumNumber_float(float x, float y)
{
#if defined(__aarch64__)
    float result;
    x = canonicalize_float(x);
    y = canonicalize_float(y);
    asm("fminnm %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return x;
    } else if (isgreater(x, y)) {
        return y;
    } else if (isunordered(x, y)) {
        x = canonicalize_float(x);
        y = canonicalize_float(y);
        return isnan(x) ? y : x; // discard NaN
    } else {
        // equal or both zero
        return signbit(x) ? x : y;
    }
#endif
}

float hs_simdy_maximumNumber_float(float x, float y)
{
#if defined(__aarch64__)
    float result;
    x = canonicalize_float(x);
    y = canonicalize_float(y);
    asm("fmaxnm %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return y;
    } else if (isgreater(x, y)) {
        return x;
    } else if (isunordered(x, y)) {
        x = canonicalize_float(x);
        y = canonicalize_float(y);
        return isnan(x) ? y : x; // discard NaN
    } else {
        // equal or both zero
        return signbit(x) ? y : x;
    }
#endif
}

#if defined(__i386__) || defined(__x86_64__)

__m128 hs_simdy_minimumNumber_floatx4(__m128 x, __m128 y)
{
    // Convert (possible) signaling NaN to quiet one
    __m128 one = _mm_set1_ps(1.0f);
    asm volatile("mulps %x1, %x0" : "+x"(x) : "x"(one));
    asm volatile("mulps %x1, %x0" : "+x"(y) : "x"(one));
    __m128 ord = _mm_cmpord_ps(x, y); // non-signaling compare
    __m128 x_ord = _mm_and_ps(x, ord); // convert possible NaN to zero (avoid undue INVALID)
    __m128 y_ord = _mm_and_ps(y, ord); // convert possible NaN to zero (avoid undue INVALID)
    __m128 result_ltgt = _mm_min_ps(x_ord, y_ord); // x_ord < y_ord ? x_ord : y_ord
    __m128 eq = _mm_cmpeq_ps(x, y); // non-signaling compare
    __m128 result_eq = _mm_and_ps(eq, x);
    __m128 result_ord = _mm_or_ps(result_eq, result_ltgt); // ordered
    __m128 x_nan_mask = _mm_cmpunord_ps(x, x);
    __m128 result_unord = _mm_or_ps(_mm_and_ps(x_nan_mask, y), _mm_andnot_ps(x_nan_mask, x)); // isnan(x) ? y : x
    return _mm_or_ps(result_ord, _mm_andnot_ps(ord, result_unord));
}

__m128 hs_simdy_maximumNumber_floatx4(__m128 x, __m128 y)
{
    // Convert (possible) signaling NaN to quiet one
    __m128 one = _mm_set1_ps(1.0f);
    asm volatile("mulps %x1, %x0" : "+x"(x) : "x"(one));
    asm volatile("mulps %x1, %x0" : "+x"(y) : "x"(one));
    __m128 ord = _mm_cmpord_ps(x, y); // non-signaling compare
    __m128 x_ord = _mm_and_ps(x, ord); // convert possible NaN to zero
    __m128 y_ord = _mm_and_ps(y, ord); // convert possible NaN to zero
    __m128 result_ltgt = _mm_max_ps(x_ord, y_ord); // x_ord > y_ord ? x_ord : y_ord
    __m128 neq = _mm_cmpneq_ps(x, y); // non-signaling (LT || GT || UNORD)
    __m128 result_ord = _mm_and_ps(_mm_or_ps(neq, x), result_ltgt);
    __m128 x_nan_mask = _mm_cmpunord_ps(x, x);
    __m128 result_unord = _mm_or_ps(_mm_and_ps(x_nan_mask, y), _mm_andnot_ps(x_nan_mask, x)); // isnan(x) ? y : x
    return _mm_or_ps(result_ord, _mm_andnot_ps(ord, result_unord));
}

__attribute__((target("avx")))
__m256 hs_simdy_minimumNumber_floatx8(__m256 xx, __m256 yy)
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

__attribute__((target("avx")))
__m256 hs_simdy_maximumNumber_floatx8(__m256 xx, __m256 yy)
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

__attribute__((target("avx512dq,avx512vl")))
__m128 hs_simdy_minimumNumber_floatx4_avx512(__m128 xx, __m128 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m128 one = _mm_set1_ps(1.0f);
    asm volatile("vmulps %x1, %x0, %x0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %x1, %x0, %x0" : "+v"(yy) : "v"(one));
    return _mm_range_ps(xx, yy, 4); // NaN is missing data
}

__attribute__((target("avx512dq,avx512vl")))
__m128 hs_simdy_maximumNumber_floatx4_avx512(__m128 xx, __m128 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m128 one = _mm_set1_ps(1.0f);
    asm volatile("vmulps %x1, %x0, %x0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %x1, %x0, %x0" : "+v"(yy) : "v"(one));
    return _mm_range_ps(xx, yy, 5); // NaN is missing data
}

__attribute__((target("avx512dq,avx512vl")))
__m256 hs_simdy_minimumNumber_floatx8_avx512(__m256 xx, __m256 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m256 one = _mm256_set1_ps(1.0f);
    asm volatile("vmulps %t1, %t0, %t0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %t1, %t0, %t0" : "+v"(yy) : "v"(one));
    return _mm256_range_ps(xx, yy, 4); // NaN is missing data
}

__attribute__((target("avx512dq,avx512vl")))
__m256 hs_simdy_maximumNumber_floatx8_avx512(__m256 xx, __m256 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m256 one = _mm256_set1_ps(1.0f);
    asm volatile("vmulps %t1, %t0, %t0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %t1, %t0, %t0" : "+v"(yy) : "v"(one));
    return _mm256_range_ps(xx, yy, 5); // NaN is missing data
}

__attribute__((target("avx512dq")))
__m512 hs_simdy_minimumNumber_floatx16(__m512 xx, __m512 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m512 one = _mm512_set1_ps(1.0f);
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(yy) : "v"(one));
    return _mm512_range_ps(xx, yy, 4); // NaN is missing data
}

__attribute__((target("avx512dq")))
__m512 hs_simdy_maximumNumber_floatx16(__m512 xx, __m512 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m512 one = _mm512_set1_ps(1.0f);
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %g1, %g0, %g0" : "+v"(yy) : "v"(one));
    return _mm512_range_ps(xx, yy, 5); // NaN is missing data
}

#elif defined(__aarch64__)

float32x4_t hs_simdy_minimumNumber_floatx4(float32x4_t x, float32x4_t y)
{
    // Convert (possible) signaling NaN to quiet one
    float32x4_t one = vdupq_n_f32(1.0f);
    asm volatile("fmul.4s %0, %0, %1" : "+w"(x) : "w"(one));
    asm volatile("fmul.4s %0, %0, %1" : "+w"(y) : "w"(one));
    // x = vmulq_f32(x, one);
    // y = vmulq_f32(y, one);
    return vminnmq_f32(x, y);
}

float32x4_t hs_simdy_maximumNumber_floatx4(float32x4_t x, float32x4_t y)
{
    // Convert (possible) signaling NaN to quiet one
    float32x4_t one = vdupq_n_f32(1.0f);
    asm volatile("fmul.4s %0, %0, %1" : "+w"(x) : "w"(one));
    asm volatile("fmul.4s %0, %0, %1" : "+w"(y) : "w"(one));
    // x = vmulq_f32(x, one);
    // y = vmulq_f32(y, one);
    return vmaxnmq_f32(x, y);
}

#endif
