#if defined(__SSE__)
#include <stdint.h>
#include <xmmintrin.h>
#include <immintrin.h>
#include "mask.h"

__m128 hs_simdy_floatx4_eq(__m128 a, __m128 b)
{
    return _mm_cmpeq_ps(a, b);
}

__m128 hs_simdy_floatx4_lt(__m128 a, __m128 b)
{
    return _mm_cmplt_ps(a, b);
}

__m128 hs_simdy_floatx4_le(__m128 a, __m128 b)
{
    return _mm_cmple_ps(a, b);
}

__m128 hs_simdy_floatx4_gt(__m128 a, __m128 b)
{
    return _mm_cmpgt_ps(a, b);
}

__m128 hs_simdy_floatx4_ge(__m128 a, __m128 b)
{
    return _mm_cmpge_ps(a, b);
}

__m128 hs_simdy_floatx4_unord(__m128 a, __m128 b)
{
    return _mm_cmpunord_ps(a, b);
}

uint8_t hs_simdy_floatx4_eq_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_EQ_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_eq(a, b));
#endif
}

uint8_t hs_simdy_floatx4_lt_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_LT_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_lt(a, b));
#endif
}

uint8_t hs_simdy_floatx4_le_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_LE_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_le(a, b));
#endif
}

uint8_t hs_simdy_floatx4_gt_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_GT_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_gt(a, b));
#endif
}

uint8_t hs_simdy_floatx4_ge_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_GE_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_ge(a, b));
#endif
}

uint8_t hs_simdy_floatx4_unord_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_UNORD_Q));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_unord(a, b));
#endif
}

#if defined(__AVX__)
__m256 hs_simdy_floatx8_eq(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_EQ_OQ);
}

__m256 hs_simdy_floatx8_lt(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_LT_OQ);
}

__m256 hs_simdy_floatx8_le(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_LE_OQ);
}

__m256 hs_simdy_floatx8_gt(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_GT_OQ);
}

__m256 hs_simdy_floatx8_ge(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_GE_OQ);
}

__m256 hs_simdy_floatx8_unord(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_UNORD_Q);
}

uint8_t hs_simdy_floatx8_eq_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_EQ_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_floatx8_eq(a, b));
#endif
}

uint8_t hs_simdy_floatx8_lt_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_LT_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_floatx8_lt(a, b));
#endif
}

uint8_t hs_simdy_floatx8_le_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_LE_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_floatx8_le(a, b));
#endif
}

uint8_t hs_simdy_floatx8_gt_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_GT_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_floatx8_gt(a, b));
#endif
}

uint8_t hs_simdy_floatx8_ge_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_GE_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_floatx8_ge(a, b));
#endif
}

uint8_t hs_simdy_floatx8_unord_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_UNORD_Q));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_floatx8_unord(a, b));
#endif
}
#endif

#if defined(__AVX512F__)
uint16_t hs_simdy_floatx16_eq_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_EQ_OQ));
}

uint16_t hs_simdy_floatx16_lt_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_LT_OQ));
}

uint16_t hs_simdy_floatx16_le_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_LE_OQ));
}

uint16_t hs_simdy_floatx16_gt_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_GT_OQ));
}

uint16_t hs_simdy_floatx16_ge_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_GE_OQ));
}

uint16_t hs_simdy_floatx16_unord_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_UNORD_Q));
}
#endif

#elif defined(__aarch64__)
#include <stdint.h>
#include <arm_neon.h>
#include "mask.h"

uint32x4_t hs_simdy_floatx4_eq(float32x4_t a, float32x4_t b)
{
    return vceqq_f32(a, b);
}

uint32x4_t hs_simdy_floatx4_lt(float32x4_t a, float32x4_t b)
{
    return vcltq_f32(a, b);
}

uint32x4_t hs_simdy_floatx4_le(float32x4_t a, float32x4_t b)
{
    return vcleq_f32(a, b);
}

uint32x4_t hs_simdy_floatx4_gt(float32x4_t a, float32x4_t b)
{
    return vcgtq_f32(a, b);
}

uint32x4_t hs_simdy_floatx4_ge(float32x4_t a, float32x4_t b)
{
    return vcgeq_f32(a, b);
}

uint8_t hs_simdy_floatx4_eq_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_eq(a, b));
}

uint8_t hs_simdy_floatx4_lt_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_lt(a, b));
}

uint8_t hs_simdy_floatx4_le_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_le(a, b));
}

uint8_t hs_simdy_floatx4_gt_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_gt(a, b));
}

uint8_t hs_simdy_floatx4_ge_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_floatx4_ge(a, b));
}

#endif
