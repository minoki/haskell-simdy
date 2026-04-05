#if defined(__AVX__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "mask.h"

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
    return hs_simdy_pack_mask32x8(hs_simdy_eqFloatX8(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_eqFloatX8_densemask_avx512(__m256 a, __m256 b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_EQ_OQ));
}

uint8_t hs_simdy_ltFloatX8_densemask(__m256 a, __m256 b)
{
    return hs_simdy_pack_mask32x8(hs_simdy_ltFloatX8(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltFloatX8_densemask_avx512(__m256 a, __m256 b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_LT_OQ));
}

uint8_t hs_simdy_leFloatX8_densemask(__m256 a, __m256 b)
{
    return hs_simdy_pack_mask32x8(hs_simdy_leFloatX8(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_leFloatX8_densemask_avx512(__m256 a, __m256 b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_LE_OQ));
}

uint8_t hs_simdy_gtFloatX8_densemask(__m256 a, __m256 b)
{
    return hs_simdy_pack_mask32x8(hs_simdy_gtFloatX8(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_gtFloatX8_densemask_avx512(__m256 a, __m256 b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_GT_OQ));
}

uint8_t hs_simdy_geFloatX8_densemask(__m256 a, __m256 b)
{
    return hs_simdy_pack_mask32x8(hs_simdy_geFloatX8(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_geFloatX8_densemask_avx512(__m256 a, __m256 b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_GE_OQ));
}

uint8_t hs_simdy_unordFloatX8_densemask(__m256 a, __m256 b)
{
    return hs_simdy_pack_mask32x8(hs_simdy_unordFloatX8(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_unordFloatX8_densemask_avx512(__m256 a, __m256 b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_ps_mask(a, b, _CMP_UNORD_Q));
}

#endif
