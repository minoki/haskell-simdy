#if defined(__i386__) || defined(__x86_64__)
#include <stdint.h>
#include <xmmintrin.h>
#include <immintrin.h>

#pragma STDC FENV_ACCESS ON

__m128 hs_simdy_minimumNumberFloatX4(__m128 x, __m128 y)
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

__m128 hs_simdy_maximumNumberFloatX4(__m128 x, __m128 y)
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

__attribute__((target("avx512vl,avx512dq")))
__m128 hs_simdy_minimumNumberFloatX4_avx512(__m128 xx, __m128 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m128 one = _mm_set1_ps(1.0f);
    asm volatile("vmulps %x1, %x0, %x0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %x1, %x0, %x0" : "+v"(yy) : "v"(one));
    return _mm_range_ps(xx, yy, 4); // NaN is missing data
}

__attribute__((target("avx512vl,avx512dq")))
__m128 hs_simdy_maximumNumberFloatX4_avx512(__m128 xx, __m128 yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m128 one = _mm_set1_ps(1.0f);
    asm volatile("vmulps %x1, %x0, %x0" : "+v"(xx) : "v"(one));
    asm volatile("vmulps %x1, %x0, %x0" : "+v"(yy) : "v"(one));
    return _mm_range_ps(xx, yy, 5); // NaN is missing data
}

#elif defined(__aarch64__)
#include <arm_neon.h>

float32x4_t hs_simdy_minimumNumberFloatX4(float32x4_t x, float32x4_t y)
{
    // Convert (possible) signaling NaN to quiet one
    float32x4_t one = vdupq_n_f32(1.0f);
    asm volatile("fmul %0.4s, %0.4s, %1.4s" : "+w"(x) : "w"(one));
    asm volatile("fmul %0.4s, %0.4s, %1.4s" : "+w"(y) : "w"(one));
    return vminnmq_f32(x, y);
}

float32x4_t hs_simdy_maximumNumberFloatX4(float32x4_t x, float32x4_t y)
{
    // Convert (possible) signaling NaN to quiet one
    float32x4_t one = vdupq_n_f32(1.0f);
    asm volatile("fmul %0.4s, %0.4s, %1.4s" : "+w"(x) : "w"(one));
    asm volatile("fmul %0.4s, %0.4s, %1.4s" : "+w"(y) : "w"(one));
    return vmaxnmq_f32(x, y);
}

#endif
