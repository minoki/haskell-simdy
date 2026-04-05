#if defined(__i386__) || defined(__x86_64__)
#include <stdint.h>
#include <xmmintrin.h>
#include <immintrin.h>

#pragma STDC FENV_ACCESS ON

__m128 hs_simdy_minimumFloatX4(__m128 x, __m128 y)
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
}

__m128 hs_simdy_maximumFloatX4(__m128 x, __m128 y)
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
}

__attribute__((target("avx512vl,avx512dq")))
__m128 hs_simdy_minimumFloatX4_avx512(__m128 xx, __m128 yy)
{
    __m128 m = _mm_range_ps(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm_mask_add_ps
    __m128 result = m;
    asm("vaddps %3, %2, %0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm_mask_add_ps(m, unord, xx, yy); // propagate NaN
#endif
}

__attribute__((target("avx512vl,avx512dq")))
__m128 hs_simdy_maximumFloatX4_avx512(__m128 xx, __m128 yy)
{
    __m128 m = _mm_range_ps(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm_cmp_ps_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm_mask_add_ps
    __m128 result = m;
    asm("vaddps %3, %2, %0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm_mask_add_ps(m, unord, xx, yy); // propagate NaN
#endif
}

#elif defined(__aarch64__)
#include <arm_neon.h>

float32x4_t hs_simdy_minimumFloatX4(float32x4_t x, float32x4_t y)
{
    return vminq_f32(x, y);
}

float32x4_t hs_simdy_maximumFloatX4(float32x4_t x, float32x4_t y)
{
    return vmaxq_f32(x, y);
}

#endif
