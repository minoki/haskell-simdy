#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "mask.h"

__m128d hs_simdy_eqDoubleX2(__m128d a, __m128d b)
{
    return _mm_cmpeq_pd(a, b);
}

__m128d hs_simdy_ltDoubleX2(__m128d a, __m128d b)
{
    return _mm_cmplt_pd(a, b);
}

__m128d hs_simdy_leDoubleX2(__m128d a, __m128d b)
{
    return _mm_cmple_pd(a, b);
}

__m128d hs_simdy_gtDoubleX2(__m128d a, __m128d b)
{
    return _mm_cmpgt_pd(a, b);
}

__m128d hs_simdy_geDoubleX2(__m128d a, __m128d b)
{
    return _mm_cmpge_pd(a, b);
}

__m128d hs_simdy_unordDoubleX2(__m128d a, __m128d b)
{
    return _mm_cmpunord_pd(a, b);
}

uint8_t hs_simdy_eqDoubleX2_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_EQ_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_eqDoubleX2(a, b));
#endif
}

uint8_t hs_simdy_ltDoubleX2_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_LT_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_ltDoubleX2(a, b));
#endif
}

uint8_t hs_simdy_leDoubleX2_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_LE_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_leDoubleX2(a, b));
#endif
}

uint8_t hs_simdy_gtDoubleX2_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_GT_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_gtDoubleX2(a, b));
#endif
}

uint8_t hs_simdy_geDoubleX2_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_GE_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_geDoubleX2(a, b));
#endif
}

uint8_t hs_simdy_unordDoubleX2_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_UNORD_Q));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_unordDoubleX2(a, b));
#endif
}

#if defined(__AVX__)
__m256d hs_simdy_eqDoubleX4(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_EQ_OQ);
}

__m256d hs_simdy_ltDoubleX4(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_LT_OQ);
}

__m256d hs_simdy_leDoubleX4(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_LE_OQ);
}

__m256d hs_simdy_gtDoubleX4(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_GT_OQ);
}

__m256d hs_simdy_geDoubleX4(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_GE_OQ);
}

__m256d hs_simdy_unordDoubleX4(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_UNORD_Q);
}

uint8_t hs_simdy_eqDoubleX4_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_EQ_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_eqDoubleX4(a, b));
#endif
}

uint8_t hs_simdy_ltDoubleX4_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_LT_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_ltDoubleX4(a, b));
#endif
}

uint8_t hs_simdy_leDoubleX4_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_LE_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_leDoubleX4(a, b));
#endif
}

uint8_t hs_simdy_gtDoubleX4_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_GT_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_gtDoubleX4(a, b));
#endif
}

uint8_t hs_simdy_geDoubleX4_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_GE_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_geDoubleX4(a, b));
#endif
}

uint8_t hs_simdy_unordDoubleX4_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_UNORD_Q));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_unordDoubleX4(a, b));
#endif
}
#endif

#if defined(__AVX512F__) && defined(__AVX512DQ__)
uint8_t hs_simdy_eqDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_EQ_OQ));
}

uint8_t hs_simdy_ltDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_LT_OQ));
}

uint8_t hs_simdy_leDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_LE_OQ));
}

uint8_t hs_simdy_gtDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_GT_OQ));
}

uint8_t hs_simdy_geDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_GE_OQ));
}

uint8_t hs_simdy_unordDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_UNORD_Q));
}
#endif

#elif defined(__aarch64__)
#include <stdint.h>
#include <arm_neon.h>
#include "mask.h"

uint64x2_t hs_simdy_eqDoubleX2(float64x2_t a, float64x2_t b)
{
    return vceqq_f64(a, b);
}

uint64x2_t hs_simdy_ltDoubleX2(float64x2_t a, float64x2_t b)
{
    return vcltq_f64(a, b);
}

uint64x2_t hs_simdy_leDoubleX2(float64x2_t a, float64x2_t b)
{
    return vcleq_f64(a, b);
}

uint64x2_t hs_simdy_gtDoubleX2(float64x2_t a, float64x2_t b)
{
    return vcgtq_f64(a, b);
}

uint64x2_t hs_simdy_geDoubleX2(float64x2_t a, float64x2_t b)
{
    return vcgeq_f64(a, b);
}

uint8_t hs_simdy_eqDoubleX2_densemask(float64x2_t a, float64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_eqDoubleX2(a, b));
}

uint8_t hs_simdy_ltDoubleX2_densemask(float64x2_t a, float64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_ltDoubleX2(a, b));
}

uint8_t hs_simdy_leDoubleX2_densemask(float64x2_t a, float64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_leDoubleX2(a, b));
}

uint8_t hs_simdy_gtDoubleX2_densemask(float64x2_t a, float64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_gtDoubleX2(a, b));
}

uint8_t hs_simdy_geDoubleX2_densemask(float64x2_t a, float64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_geDoubleX2(a, b));
}

#endif
