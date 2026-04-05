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
    return hs_simdy_pack_mask32x4(hs_simdy_eqFloatX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_eqFloatX4_densemask_avx512(__m128 a, __m128 b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_EQ_OQ));
}

uint8_t hs_simdy_ltFloatX4_densemask(__m128 a, __m128 b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_ltFloatX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltFloatX4_densemask_avx512(__m128 a, __m128 b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_LT_OQ));
}

uint8_t hs_simdy_leFloatX4_densemask(__m128 a, __m128 b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_leFloatX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_leFloatX4_densemask_avx512(__m128 a, __m128 b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_LE_OQ));
}

uint8_t hs_simdy_gtFloatX4_densemask(__m128 a, __m128 b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_gtFloatX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_gtFloatX4_densemask_avx512(__m128 a, __m128 b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_GT_OQ));
}

uint8_t hs_simdy_geFloatX4_densemask(__m128 a, __m128 b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_geFloatX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_geFloatX4_densemask_avx512(__m128 a, __m128 b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_GE_OQ));
}

uint8_t hs_simdy_unordFloatX4_densemask(__m128 a, __m128 b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_unordFloatX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_unordFloatX4_densemask_avx512(__m128 a, __m128 b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_ps_mask(a, b, _CMP_UNORD_Q));
}

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
