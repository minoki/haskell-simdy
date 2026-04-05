#if defined(__AVX2__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "mask.h"

__m256i hs_simdy_eqInt8X32(__m256i a, __m256i b)
{
    return _mm256_cmpeq_epi8(a, b);
}

__m256i hs_simdy_eqInt16X16(__m256i a, __m256i b)
{
    return _mm256_cmpeq_epi16(a, b);
}

__m256i hs_simdy_eqInt32X8(__m256i a, __m256i b)
{
    return _mm256_cmpeq_epi32(a, b);
}

__m256i hs_simdy_eqInt64X4(__m256i a, __m256i b)
{
    return _mm256_cmpeq_epi64(a, b);
}

uint32_t hs_simdy_eqInt8X32_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask8x32(hs_simdy_eqInt8X32(a, b));
}

__attribute__((target("avx512vl,avx512bw")))
uint32_t hs_simdy_eqInt8X32_densemask_avx512(__m256i a, __m256i b)
{
    return _cvtmask32_u32(_mm256_cmpeq_epi8_mask(a, b));
}

uint16_t hs_simdy_eqInt16X16_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask16x16(hs_simdy_eqInt16X16(a, b));
}

__attribute__((target("avx512vl,avx512bw")))
uint16_t hs_simdy_eqInt16X16_densemask_avx512(__m256i a, __m256i b)
{
    return (uint16_t)_cvtmask16_u32(_mm256_cmpeq_epi16_mask(a, b));
}

uint8_t hs_simdy_eqInt32X8_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask32x8(_mm256_castsi256_ps(hs_simdy_eqInt32X8(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_eqInt32X8_densemask_avx512(__m256i a, __m256i b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmpeq_epi32_mask(a, b));
}

uint8_t hs_simdy_eqInt64X4_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask64x4(_mm256_castsi256_pd(hs_simdy_eqInt64X4(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_eqInt64X4_densemask_avx512(__m256i a, __m256i b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmpeq_epi64_mask(a, b));
}

__m256i hs_simdy_ltInt8X32(__m256i a, __m256i b)
{
    return _mm256_cmpgt_epi8(b, a);
}

__m256i hs_simdy_ltInt16X16(__m256i a, __m256i b)
{
    return _mm256_cmpgt_epi16(b, a);
}

__m256i hs_simdy_ltInt32X8(__m256i a, __m256i b)
{
    return _mm256_cmpgt_epi32(b, a);
}

__m256i hs_simdy_ltInt64X4(__m256i a, __m256i b)
{
    return _mm256_cmpgt_epi64(b, a);
}

uint32_t hs_simdy_ltInt8X32_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask8x32(hs_simdy_ltInt8X32(a, b));
}

__attribute__((target("avx512vl,avx512bw")))
uint32_t hs_simdy_ltInt8X32_densemask_avx512(__m256i a, __m256i b)
{
    return _cvtmask32_u32(_mm256_cmplt_epi8_mask(a, b));
}

uint16_t hs_simdy_ltInt16X16_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask16x16(hs_simdy_ltInt16X16(a, b));
}

__attribute__((target("avx512vl,avx512bw")))
uint16_t hs_simdy_ltInt16X16_densemask_avx512(__m256i a, __m256i b)
{
    return (uint16_t)_cvtmask16_u32(_mm256_cmplt_epi16_mask(a, b));
}

uint8_t hs_simdy_ltInt32X8_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask32x8(_mm256_castsi256_ps(hs_simdy_ltInt32X8(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltInt32X8_densemask_avx512(__m256i a, __m256i b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmplt_epi32_mask(a, b));
}

uint8_t hs_simdy_ltInt64X4_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask64x4(_mm256_castsi256_pd(hs_simdy_ltInt64X4(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltInt64X4_densemask_avx512(__m256i a, __m256i b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmplt_epi64_mask(a, b));
}

__m256i hs_simdy_ltWord8X32(__m256i a, __m256i b)
{
    __m256i mask = _mm256_set1_epi8(INT8_MIN);
    a = _mm256_xor_si256(a, mask);
    b = _mm256_xor_si256(b, mask);
    return _mm256_cmpgt_epi8(b, a);
}

__m256i hs_simdy_ltWord16X16(__m256i a, __m256i b)
{
    __m256i mask = _mm256_set1_epi16(INT16_MIN);
    a = _mm256_xor_si256(a, mask);
    b = _mm256_xor_si256(b, mask);
    return _mm256_cmpgt_epi16(b, a);
}

__m256i hs_simdy_ltWord32X8(__m256i a, __m256i b)
{
    __m256i mask = _mm256_set1_epi32(INT32_MIN);
    a = _mm256_xor_si256(a, mask);
    b = _mm256_xor_si256(b, mask);
    return _mm256_cmpgt_epi32(b, a);
}

__m256i hs_simdy_ltWord64X4(__m256i a, __m256i b)
{
    __m256i mask = _mm256_set1_epi64x(INT64_MIN);
    a = _mm256_xor_si256(a, mask);
    b = _mm256_xor_si256(b, mask);
    return _mm256_cmpgt_epi64(b, a);
}

uint32_t hs_simdy_ltWord8X32_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask8x32(hs_simdy_ltWord8X32(a, b));
}

__attribute__((target("avx512vl,avx512bw")))
uint32_t hs_simdy_ltWord8X32_densemask_avx512(__m256i a, __m256i b)
{
    return _cvtmask32_u32(_mm256_cmplt_epu8_mask(a, b));
}

uint16_t hs_simdy_ltWord16X16_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask16x16(hs_simdy_ltWord16X16(a, b));
}

__attribute__((target("avx512vl,avx512bw")))
uint16_t hs_simdy_ltWord16X16_densemask_avx512(__m256i a, __m256i b)
{
    return (uint16_t)_cvtmask16_u32(_mm256_cmplt_epu16_mask(a, b));
}

uint8_t hs_simdy_ltWord32X8_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask32x8(_mm256_castsi256_ps(hs_simdy_ltWord32X8(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltWord32X8_densemask_avx512(__m256i a, __m256i b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmplt_epu32_mask(a, b));
}

uint8_t hs_simdy_ltWord64X4_densemask(__m256i a, __m256i b)
{
    return hs_simdy_pack_mask64x4(_mm256_castsi256_pd(hs_simdy_ltWord64X4(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltWord64X4_densemask_avx512(__m256i a, __m256i b)
{
    return (uint8_t)_cvtmask8_u32(_mm256_cmplt_epu64_mask(a, b));
}

#endif
