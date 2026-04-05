#if defined(__i386__) || defined(__x86_64__)
#include <stdint.h>
#include <xmmintrin.h>
#include <immintrin.h>

__m128d hs_simdy_minimumNumberDoubleX2(__m128d x, __m128d y)
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

__m128d hs_simdy_maximumNumberDoubleX2(__m128d x, __m128d y)
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

__attribute__((target("avx512vl,avx512dq")))
__m128d hs_simdy_minimumNumberDoubleX2_avx512(__m128d xx, __m128d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m128d one = _mm_set1_pd(1.0);
    asm volatile("vmulpd %x1, %x0, %x0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %x1, %x0, %x0" : "+v"(yy) : "v"(one));
    return _mm_range_pd(xx, yy, 4); // NaN is missing data
}

__attribute__((target("avx512vl,avx512dq")))
__m128d hs_simdy_maximumNumberDoubleX2_avx512(__m128d xx, __m128d yy)
{
    // Convert (possible) signaling NaN to quiet one
    __m128d one = _mm_set1_pd(1.0);
    asm volatile("vmulpd %x1, %x0, %x0" : "+v"(xx) : "v"(one));
    asm volatile("vmulpd %x1, %x0, %x0" : "+v"(yy) : "v"(one));
    return _mm_range_pd(xx, yy, 5); // NaN is missing data
}

#elif defined(__aarch64__)
#include <arm_neon.h>

float64x2_t hs_simdy_minimumNumberDoubleX2(float64x2_t x, float64x2_t y)
{
    // Convert (possible) signaling NaN to quiet one
    float64x2_t one = vdupq_n_f64(1.0);
    asm volatile("fmul %0.2d, %0.2d, %1.2d" : "+w"(x) : "w"(one));
    asm volatile("fmul %0.2d, %0.2d, %1.2d" : "+w"(y) : "w"(one));
    // x = vmulq_f32(x, one);
    // y = vmulq_f32(y, one);
    return vminnmq_f64(x, y);
}

float64x2_t hs_simdy_maximumNumberDoubleX2(float64x2_t x, float64x2_t y)
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
