#include "mask.h"

#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>

uint16_t hs_simdy_pack_mask8x16(__m128i v)
{
    return (uint16_t)_mm_movemask_epi8(v);
}

uint8_t hs_simdy_pack_mask16x8(__m128i v)
{
    __m128i zero = _mm_set1_epi16(0);
    __m128i mask8x16 = _mm_packs_epi16(v, zero);
    return (uint8_t)_mm_movemask_epi8(mask8x16);
}

uint8_t hs_simdy_pack_mask32x4(__m128 v)
{
    return (uint8_t)_mm_movemask_ps(v);
}

uint8_t hs_simdy_pack_mask64x2(__m128d v)
{
    return (uint8_t)_mm_movemask_pd(v);
}

#if defined(__AVX2__)
uint32_t hs_simdy_pack_mask8x32(__m256i v)
{
    return (uint32_t)_mm256_movemask_epi8(v);
}

uint16_t hs_simdy_pack_mask16x16(__m256i v)
{
    __m256i zero = _mm256_setzero_si256();
    __m256i mask8x32 = _mm256_packs_epi16(v, zero);
    return (uint16_t)_mm256_movemask_epi8(mask8x32);
}

uint8_t hs_simdy_pack_mask32x8(__m256 v)
{
    return (uint8_t)_mm256_movemask_ps(v);
}

uint8_t hs_simdy_pack_mask64x4(__m256d v)
{
    return (uint8_t)_mm256_movemask_pd(v);
}
#endif

__m128i hs_simdy_mask16x8x2_to_mask8x16(__m128i lo, __m128i hi)
{
    return _mm_packs_epi16(lo, hi);
}

__m128i hs_simdy_mask32x4x2_to_mask16x8(__m128i lo, __m128i hi)
{
    return _mm_packs_epi32(lo, hi);
}

__m128 hs_simdy_mask64x2x2_to_mask32x4(__m128 lo, __m128 hi)
{
    return _mm_shuffle_ps(lo, hi, 0xe4); // 0b11_10_01_00
}

extern __m128i hs_simdy_unpack_mask8x16(uint16_t i);
extern __m128i hs_simdy_unpack_mask16x8(uint8_t i);
const _Alignas(16) int32_t hs_simdy_mask32x4_table[4 * 16] = {
    0, 0, 0, 0,
    -1, 0, 0, 0,
    0, -1, 0, 0,
    -1, -1, 0, 0,
    0, 0, -1, 0,
    -1, 0, -1, 0,
    0, -1, -1, 0,
    -1, -1, -1, 0,
    0, 0, 0, -1,
    -1, 0, 0, -1,
    0, -1, 0, -1,
    -1, -1, 0, -1,
    0, 0, -1, -1,
    -1, 0, -1, -1,
    0, -1, -1, -1,
    -1, -1, -1, -1,
};
extern __m128i hs_simdy_unpack_mask32x4(uint8_t i);

const _Alignas(16) int64_t hs_simdy_mask64x2_table[8] = {
    0, 0, -1, 0, 0, -1, -1, -1,
};
extern __m128i hs_simdy_unpack_mask64x2(uint8_t i);

#if defined(__AVX2__)
extern __m256i hs_simdy_unpack_mask8x32(uint32_t i);
extern __m256i hs_simdy_unpack_mask16x16(uint16_t i);
extern __m256i hs_simdy_unpack_mask32x8(uint8_t i);

const _Alignas(32) int64_t hs_simdy_mask64x4_table[4 * 16] = {
    0, 0, 0, 0,
    -1, 0, 0, 0,
    0, -1, 0, 0,
    -1, -1, 0, 0,
    0, 0, -1, 0,
    -1, 0, -1, 0,
    0, -1, -1, 0,
    -1, -1, -1, 0,
    0, 0, 0, -1,
    -1, 0, 0, -1,
    0, -1, 0, -1,
    -1, -1, 0, -1,
    0, 0, -1, -1,
    -1, 0, -1, -1,
    0, -1, -1, -1,
    -1, -1, -1, -1,
};
extern __m256i hs_simdy_unpack_mask64x4(uint8_t i);
#endif

#endif
