#if defined(__SSE__)
#include <stdint.h>
#include <xmmintrin.h>
#include <immintrin.h>
#include "mask.h"

__m128 hs_simdy_eqFloatX4(__m128 a, __m128 b)
{
    return _mm_cmpeq_ps(a, b);
}

__m128 hs_simdy_ltFloatX4(__m128 a, __m128 b)
{
    return _mm_cmplt_ps(a, b);
}

__m128 hs_simdy_leFloatX4(__m128 a, __m128 b)
{
    return _mm_cmple_ps(a, b);
}

__m128 hs_simdy_gtFloatX4(__m128 a, __m128 b)
{
    return _mm_cmpgt_ps(a, b);
}

__m128 hs_simdy_geFloatX4(__m128 a, __m128 b)
{
    return _mm_cmpge_ps(a, b);
}

__m128 hs_simdy_unordFloatX4(__m128 a, __m128 b)
{
    return _mm_cmpunord_ps(a, b);
}

uint8_t hs_simdy_eqFloatX4_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_EQ_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_eqFloatX4(a, b));
#endif
}

uint8_t hs_simdy_ltFloatX4_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_LT_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_ltFloatX4(a, b));
#endif
}

uint8_t hs_simdy_leFloatX4_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_LE_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_leFloatX4(a, b));
#endif
}

uint8_t hs_simdy_gtFloatX4_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_GT_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_gtFloatX4(a, b));
#endif
}

uint8_t hs_simdy_geFloatX4_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_GE_OQ));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_geFloatX4(a, b));
#endif
}

uint8_t hs_simdy_unordFloatX4_densemask(__m128 a, __m128 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_UNORD_Q));
#else
    return hs_simdy_pack_mask32x4(hs_simdy_unordFloatX4(a, b));
#endif
}

#if defined(__AVX__)
__m256 hs_simdy_eqFloatX8(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_EQ_OQ);
}

__m256 hs_simdy_ltFloatX8(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_LT_OQ);
}

__m256 hs_simdy_leFloatX8(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_LE_OQ);
}

__m256 hs_simdy_gtFloatX8(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_GT_OQ);
}

__m256 hs_simdy_geFloatX8(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_GE_OQ);
}

__m256 hs_simdy_unordFloatX8(__m256 a, __m256 b)
{
    return _mm256_cmp_ps(a, b, _CMP_UNORD_Q);
}

uint8_t hs_simdy_eqFloatX8_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_EQ_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_eqFloatX8(a, b));
#endif
}

uint8_t hs_simdy_ltFloatX8_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_LT_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_ltFloatX8(a, b));
#endif
}

uint8_t hs_simdy_leFloatX8_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_LE_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_leFloatX8(a, b));
#endif
}

uint8_t hs_simdy_gtFloatX8_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_GT_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_gtFloatX8(a, b));
#endif
}

uint8_t hs_simdy_geFloatX8_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_GE_OQ));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_geFloatX8(a, b));
#endif
}

uint8_t hs_simdy_unordFloatX8_densemask(__m256 a, __m256 b)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_UNORD_Q));
#else
    return hs_simdy_pack_mask32x8(hs_simdy_unordFloatX8(a, b));
#endif
}
#endif

#if defined(__AVX512F__)
uint16_t hs_simdy_eqFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_EQ_OQ));
}

uint16_t hs_simdy_ltFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_LT_OQ));
}

uint16_t hs_simdy_leFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_LE_OQ));
}

uint16_t hs_simdy_gtFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_GT_OQ));
}

uint16_t hs_simdy_geFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_GE_OQ));
}

uint16_t hs_simdy_unordFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_UNORD_Q));
}
#endif

#elif defined(__aarch64__)
#include <stdint.h>
#include <arm_neon.h>
#include "mask.h"

uint32x4_t hs_simdy_eqFloatX4(float32x4_t a, float32x4_t b)
{
    return vceqq_f32(a, b);
}

uint32x4_t hs_simdy_ltFloatX4(float32x4_t a, float32x4_t b)
{
    return vcltq_f32(a, b);
}

uint32x4_t hs_simdy_leFloatX4(float32x4_t a, float32x4_t b)
{
    return vcleq_f32(a, b);
}

uint32x4_t hs_simdy_gtFloatX4(float32x4_t a, float32x4_t b)
{
    return vcgtq_f32(a, b);
}

uint32x4_t hs_simdy_geFloatX4(float32x4_t a, float32x4_t b)
{
    return vcgeq_f32(a, b);
}

uint8_t hs_simdy_eqFloatX4_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_eqFloatX4(a, b));
}

uint8_t hs_simdy_ltFloatX4_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_ltFloatX4(a, b));
}

uint8_t hs_simdy_leFloatX4_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_leFloatX4(a, b));
}

uint8_t hs_simdy_gtFloatX4_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_gtFloatX4(a, b));
}

uint8_t hs_simdy_geFloatX4_densemask(float32x4_t a, float32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_geFloatX4(a, b));
}

#endif
