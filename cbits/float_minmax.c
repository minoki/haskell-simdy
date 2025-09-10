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

float hs_simdy_minimum_float(float x, float y)
{
#if defined(__aarch64__)
    float result;
    asm("fmin %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
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

float hs_simdy_maximum_float(float x, float y)
{
#if defined(__aarch64__)
    float result;
    asm("fmax %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
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

__m128 hs_simdy_minimum_floatx4(__m128 x, __m128 y)
{
    __m128 ord = _mm_cmpord_ps(x, y); // non-signaling compare
    __m128 x_ord = _mm_and_ps(x, ord); // convert possible NaN to zero
    __m128 y_ord = _mm_and_ps(y, ord); // convert possible NaN to zero
    __m128 result_ltgt = _mm_min_ps(x_ord, y_ord); // x_ord < y_ord ? x_ord : y_ord
    __m128 eq = _mm_cmpeq_ps(x, y); // non-signaling compare
    __m128 result_eq = _mm_and_ps(eq, x);
    __m128 result_ord = _mm_or_ps(result_eq, result_ltgt); // ordered
    __m128 x_unord = _mm_andnot_ps(ord, x); // ~ord & x
    __m128 y_unord = _mm_andnot_ps(ord, y); // ~ord & y
    __m128 result_unord = _mm_add_ps(x_unord, y_unord); // propagate NaN
    return _mm_or_ps(result_ord, result_unord);
    /*
    float x0, y0;
    _mm_store_ss(&x0, x);
    _mm_store_ss(&y0, y);
    float z0 = hs_simdy_minimum_float(x0, y0);
    float x1, y1;
    _mm_store_ss(&x1, _mm_shuffle_ps(x, x, 1));
    _mm_store_ss(&y1, _mm_shuffle_ps(y, y, 1));
    float z1 = hs_simdy_minimum_float(x1, y1);
    float x2, y2;
    _mm_store_ss(&x2, _mm_shuffle_ps(x, x, 2));
    _mm_store_ss(&y2, _mm_shuffle_ps(y, y, 2));
    float z2 = hs_simdy_minimum_float(x2, y2);
    float x3, y3;
    _mm_store_ss(&x3, _mm_shuffle_ps(x, x, 3));
    _mm_store_ss(&y3, _mm_shuffle_ps(y, y, 3));
    float z3 = hs_simdy_minimum_float(x3, y3);
    return _mm_set_ps(z3, z2, z1, z0);
    */
}

__m128 hs_simdy_maximum_floatx4(__m128 x, __m128 y)
{
    __m128 ord = _mm_cmpord_ps(x, y); // non-signaling compare
    __m128 x_ord = _mm_and_ps(x, ord); // convert possible NaN to zero
    __m128 y_ord = _mm_and_ps(y, ord); // convert possible NaN to zero
    __m128 result_ltgt = _mm_max_ps(x_ord, y_ord); // x_ord > y_ord ? x_ord : y_ord
    __m128 neq = _mm_cmpneq_ps(x, y); // non-signaling (LT || GT || UNORD)
    __m128 result_ord = _mm_and_ps(_mm_or_ps(neq, x), result_ltgt);
    __m128 x_unord = _mm_andnot_ps(ord, x); // ~ord & x
    __m128 y_unord = _mm_andnot_ps(ord, y); // ~ord & y
    __m128 result_unord = _mm_add_ps(x_unord, y_unord); // propagate NaN
    return _mm_or_ps(result_ord, result_unord);
    /*
    float x0, y0;
    _mm_store_ss(&x0, x);
    _mm_store_ss(&y0, y);
    float z0 = hs_simdy_maximum_float(x0, y0);
    float x1, y1;
    _mm_store_ss(&x1, _mm_shuffle_ps(x, x, 1));
    _mm_store_ss(&y1, _mm_shuffle_ps(y, y, 1));
    float z1 = hs_simdy_maximum_float(x1, y1);
    float x2, y2;
    _mm_store_ss(&x2, _mm_shuffle_ps(x, x, 2));
    _mm_store_ss(&y2, _mm_shuffle_ps(y, y, 2));
    float z2 = hs_simdy_maximum_float(x2, y2);
    float x3, y3;
    _mm_store_ss(&x3, _mm_shuffle_ps(x, x, 3));
    _mm_store_ss(&y3, _mm_shuffle_ps(y, y, 3));
    float z3 = hs_simdy_maximum_float(x3, y3);
    return _mm_set_ps(z3, z2, z1, z0);
    */
}

#if 0
__attribute__((target("avx")))
__m128 hs_simdy_minimum_floatx4_avx(__m128 xx, __m128 yy)
{
    __m128 unord_mask = _mm_cmpunord_ps(xx, yy);
    if (!_mm_testz_ps(unord_mask, unord_mask)) {
        // Some NaN in input
        return hs_simdy_minimum_floatx4(xx, yy);
    } else {
        // No NaN in input
        __m128 neq = _mm_cmp_ps(xx, yy, 0xc); // not-equal, non-signaling (AVX feature)
        __m128 zz_neq = _mm_min_ps(xx, yy);
        __m128 zz_eq = _mm_or_ps(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        // __m128 zz = _mm_or_ps(_mm_and_ps(neq, zz_neq), _mm_andnot_ps(neq, zz_eq));
        return _mm_blendv_ps(zz_eq, zz_neq, neq);
    }
}

__attribute__((target("avx")))
__m128 hs_simdy_maximum_floatx4_avx(__m128 xx, __m128 yy)
{
    __m128 unord_mask = _mm_cmpunord_ps(xx, yy);
    if (!_mm_testz_ps(unord_mask, unord_mask)) {
        // Some NaN in input
        return hs_simdy_maximum_floatx4(xx, yy);
    } else {
        // No NaN in input
        __m128 neq = _mm_cmp_ps(xx, yy, 0xc); // not-equal, non-signaling (AVX feature)
        __m128 zz_neq = _mm_max_ps(xx, yy);
        __m128 zz_eq = _mm_and_ps(xx, yy);
        // neq ? zz_neq : zz_eq = (neq & zz_neq) | (~neq & zz_eq)
        // __m128 zz = _mm_or_ps(_mm_and_ps(neq, zz_neq), _mm_andnot_ps(neq, zz_eq));
        return _mm_blendv_ps(zz_eq, zz_neq, neq);
    }
}
#endif

__attribute__((target("avx")))
__m256 hs_simdy_minimum_floatx8(__m256 xx, __m256 yy)
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

__attribute__((target("avx")))
__m256 hs_simdy_maximum_floatx8(__m256 xx, __m256 yy)
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

__attribute__((target("avx512dq,avx512vl")))
__m128 hs_simdy_minimum_floatx4_avx512(__m128 xx, __m128 yy)
{
    __m128 m = _mm_range_ps(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm_mask_add_ps(m, unord, xx, yy); // propagate NaN
    /*
    __m128 unord = _mm_cmp_ps(xx, yy, 0x3);
    __m128 a = _mm_or_ps(xx, yy); // propagate NaN
    __m128 m = _mm_range_ps(xx, yy, 4);
    return _mm_blendv_ps(m, a, unord);
    */
}

__attribute__((target("avx512dq,avx512vl")))
__m128 hs_simdy_maximum_floatx4_avx512(__m128 xx, __m128 yy)
{
    __m128 m = _mm_range_ps(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm_mask_add_ps(m, unord, xx, yy); // propagate NaN
    /*
    __m128 unord = _mm_cmp_ps(xx, yy, 0x3);
    __m128 a = _mm_or_ps(xx, yy); // propagate NaN
    __m128 m = _mm_range_ps(xx, yy, 5);
    return _mm_blendv_ps(m, a, unord);
    */
}

__attribute__((target("avx512dq,avx512vl")))
__m256 hs_simdy_minimum_floatx8_avx512(__m256 xx, __m256 yy)
{
    __m256 m = _mm256_range_ps(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm256_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm256_mask_add_ps(m, unord, xx, yy); // propagate NaN
    /*
    __m256 unord = _mm256_cmp_ps(xx, yy, 0x3);
    __m256 a = _mm256_or_ps(xx, yy); // propagate NaN
    __m256 m = _mm256_range_ps(xx, yy, 4);
    return _mm256_blendv_ps(m, a, unord);
    */
}

__attribute__((target("avx512dq,avx512vl")))
__m256 hs_simdy_maximum_floatx8_avx512(__m256 xx, __m256 yy)
{
    __m256 m = _mm256_range_ps(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm256_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm256_mask_add_ps(m, unord, xx, yy); // propagate NaN
    /*
    __m256 unord = _mm256_cmp_ps(xx, yy, 0x3);
    __m256 a = _mm256_or_ps(xx, yy); // propagate NaN
    __m256 m = _mm256_range_ps(xx, yy, 5);
    return _mm256_blendv_ps(m, a, unord);
    */
}

__attribute__((target("avx512dq")))
__m512 hs_simdy_minimum_floatx16(__m512 xx, __m512 yy)
{
    __m512 m = _mm512_range_ps(xx, yy, 4); // NaN is missing data
    __mmask16 unord = _mm512_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm512_mask_add_ps(m, unord, xx, yy); // propagate NaN
}

__attribute__((target("avx512dq")))
__m512 hs_simdy_maximum_floatx16(__m512 xx, __m512 yy)
{
    __m512 m = _mm512_range_ps(xx, yy, 5); // NaN is missing data
    __mmask16 unord = _mm512_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
    return _mm512_mask_add_ps(m, unord, xx, yy); // propagate NaN
}

#elif defined(__aarch64__)

float32x4_t hs_simdy_minimum_floatx4(float32x4_t x, float32x4_t y)
{
    return vminq_f32(x, y);
}

float32x4_t hs_simdy_maximum_floatx4(float32x4_t x, float32x4_t y)
{
    return vmaxq_f32(x, y);
}

#endif
