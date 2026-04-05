#if defined(__i386__) || defined(__x86_64__)
#include <stdint.h>
#include <xmmintrin.h>
#include <immintrin.h>

__m128d hs_simdy_minimumDoubleX2(__m128d x, __m128d y)
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

__m128d hs_simdy_maximumDoubleX2(__m128d x, __m128d y)
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

__attribute__((target("avx512vl,avx512dq")))
__m128d hs_simdy_minimumDoubleX2_avx512(__m128d xx, __m128d yy)
{
    __m128d m = _mm_range_pd(xx, yy, 4); // NaN is missing data
    __mmask8 unord = _mm_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm_mask_add_pd
    __m128d result = m;
    asm("vaddpd %3, %2, %0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm_mask_add_pd(m, unord, xx, yy); // propagate NaN
#endif
}

__attribute__((target("avx512vl,avx512dq")))
__m128d hs_simdy_maximumDoubleX2_avx512(__m128d xx, __m128d yy)
{
    __m128d m = _mm_range_pd(xx, yy, 5); // NaN is missing data
    __mmask8 unord = _mm_cmp_pd_mask(xx, yy, 0x3); // UNORD (quiet)
#if defined(__clang__)
    // LLVM may emit add+mov for _mm_mask_add_pd
    __m128d result = m;
    asm("vaddpd %3, %2, %0 %{%1%}" : "+v"(result) : "Yk"(unord), "v"(xx), "v"(yy));
    return result;
#else
    return _mm_mask_add_pd(m, unord, xx, yy); // propagate NaN
#endif
}

#elif defined(__aarch64__)
#include <arm_neon.h>

float64x2_t hs_simdy_minimumDoubleX2(float64x2_t x, float64x2_t y)
{
    return vminq_f64(x, y);
}

float64x2_t hs_simdy_maximumDoubleX2(float64x2_t x, float64x2_t y)
{
    return vmaxq_f64(x, y);
}

#endif
