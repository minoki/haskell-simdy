#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "mask.h"

__m128d hs_simdy_doublex2_eq(__m128d a, __m128d b)
{
    return _mm_cmpeq_pd(a, b);
}

__m128d hs_simdy_doublex2_lt(__m128d a, __m128d b)
{
    return _mm_cmplt_pd(a, b);
}

__m128d hs_simdy_doublex2_le(__m128d a, __m128d b)
{
    return _mm_cmple_pd(a, b);
}

__m128d hs_simdy_doublex2_gt(__m128d a, __m128d b)
{
    return _mm_cmpgt_pd(a, b);
}

__m128d hs_simdy_doublex2_ge(__m128d a, __m128d b)
{
    return _mm_cmpge_pd(a, b);
}

__m128d hs_simdy_doublex2_unord(__m128d a, __m128d b)
{
    return _mm_cmpunord_pd(a, b);
}

uint8_t hs_simdy_doublex2_eq_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_EQ_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_doublex2_eq(a, b));
#endif
}

uint8_t hs_simdy_doublex2_lt_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_LT_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_doublex2_lt(a, b));
#endif
}

uint8_t hs_simdy_doublex2_le_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_LE_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_doublex2_le(a, b));
#endif
}

uint8_t hs_simdy_doublex2_gt_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_GT_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_doublex2_gt(a, b));
#endif
}

uint8_t hs_simdy_doublex2_ge_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_GE_OQ));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_doublex2_ge(a, b));
#endif
}

uint8_t hs_simdy_doublex2_unord_densemask(__m128d a, __m128d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_UNORD_Q));
#else
    return hs_simdy_pack_mask64x2(hs_simdy_doublex2_unord(a, b));
#endif
}

#if defined(__AVX__)
__m256d hs_simdy_doublex4_eq(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_EQ_OQ);
}

__m256d hs_simdy_doublex4_lt(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_LT_OQ);
}

__m256d hs_simdy_doublex4_le(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_LE_OQ);
}

__m256d hs_simdy_doublex4_gt(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_GT_OQ);
}

__m256d hs_simdy_doublex4_ge(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_GE_OQ);
}

__m256d hs_simdy_doublex4_unord(__m256d a, __m256d b)
{
    return _mm256_cmp_pd(a, b, _CMP_UNORD_Q);
}

uint8_t hs_simdy_doublex4_eq_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_EQ_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_doublex4_eq(a, b));
#endif
}

uint8_t hs_simdy_doublex4_lt_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_LT_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_doublex4_lt(a, b));
#endif
}

uint8_t hs_simdy_doublex4_le_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_LE_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_doublex4_le(a, b));
#endif
}

uint8_t hs_simdy_doublex4_gt_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_GT_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_doublex4_gt(a, b));
#endif
}

uint8_t hs_simdy_doublex4_ge_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_GE_OQ));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_doublex4_ge(a, b));
#endif
}

uint8_t hs_simdy_doublex4_unord_densemask(__m256d a, __m256d b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_UNORD_Q));
#else
    return hs_simdy_pack_mask64x4(hs_simdy_doublex4_unord(a, b));
#endif
}
#endif

#if defined(__AVX512F__) && defined(__AVX512DQ__)
uint8_t hs_simdy_doublex8_eq_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_EQ_OQ));
}

uint8_t hs_simdy_doublex8_lt_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_LT_OQ));
}

uint8_t hs_simdy_doublex8_le_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_LE_OQ));
}

uint8_t hs_simdy_doublex8_gt_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_GT_OQ));
}

uint8_t hs_simdy_doublex8_ge_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_GE_OQ));
}

uint8_t hs_simdy_doublex8_unord_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_UNORD_Q));
}
#endif

#endif
