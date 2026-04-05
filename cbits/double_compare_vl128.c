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
    return hs_simdy_pack_mask64x2(hs_simdy_eqDoubleX2(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_eqDoubleX2_densemask_avx512(__m128d a, __m128d b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_EQ_OQ));
}

uint8_t hs_simdy_ltDoubleX2_densemask(__m128d a, __m128d b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_ltDoubleX2(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltDoubleX2_densemask_avx512(__m128d a, __m128d b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_LT_OQ));
}

uint8_t hs_simdy_leDoubleX2_densemask(__m128d a, __m128d b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_leDoubleX2(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_leDoubleX2_densemask_avx512(__m128d a, __m128d b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_LE_OQ));
}

uint8_t hs_simdy_gtDoubleX2_densemask(__m128d a, __m128d b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_gtDoubleX2(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_gtDoubleX2_densemask_avx512(__m128d a, __m128d b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_GT_OQ));
}

uint8_t hs_simdy_geDoubleX2_densemask(__m128d a, __m128d b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_geDoubleX2(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_geDoubleX2_densemask_avx512(__m128d a, __m128d b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_GE_OQ));
}

uint8_t hs_simdy_unordDoubleX2_densemask(__m128d a, __m128d b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_unordDoubleX2(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_unordDoubleX2_densemask_avx512(__m128d a, __m128d b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmp_pd_mask(a, b, _CMP_UNORD_Q));
}

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
