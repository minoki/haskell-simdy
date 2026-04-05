#if defined(__AVX__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "mask.h"

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
    return hs_simdy_pack_mask64x4(hs_simdy_eqDoubleX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_eqDoubleX4_densemask_avx512(__m256d a, __m256d b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_EQ_OQ));
}

uint8_t hs_simdy_ltDoubleX4_densemask(__m256d a, __m256d b)
{
    return hs_simdy_pack_mask64x4(hs_simdy_ltDoubleX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltDoubleX4_densemask_avx512(__m256d a, __m256d b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_LT_OQ));
}

uint8_t hs_simdy_leDoubleX4_densemask(__m256d a, __m256d b)
{
    return hs_simdy_pack_mask64x4(hs_simdy_leDoubleX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_leDoubleX4_densemask_avx512(__m256d a, __m256d b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_LE_OQ));
}

uint8_t hs_simdy_gtDoubleX4_densemask(__m256d a, __m256d b)
{
    return hs_simdy_pack_mask64x4(hs_simdy_gtDoubleX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_gtDoubleX4_densemask_avx512(__m256d a, __m256d b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_GT_OQ));
}

uint8_t hs_simdy_geDoubleX4_densemask(__m256d a, __m256d b)
{
    return hs_simdy_pack_mask64x4(hs_simdy_geDoubleX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_geDoubleX4_densemask_avx512(__m256d a, __m256d b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_GE_OQ));
}

uint8_t hs_simdy_unordDoubleX4_densemask(__m256d a, __m256d b)
{
    return hs_simdy_pack_mask64x4(hs_simdy_unordDoubleX4(a, b));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_unordDoubleX4_densemask_avx512(__m256d a, __m256d b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmp_pd_mask(a, b, _CMP_UNORD_Q));
}

#endif
