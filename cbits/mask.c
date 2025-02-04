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

__m128i hs_simdy_unpack_mask8x16(uint16_t i)
{
    __m128i ii_lo = _mm_set1_epi8(i & 0xFF);
    __m128i ii_hi = _mm_set1_epi8(i >> 8);
    __m128i mask_lo = _mm_set_epi8(0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01);
    __m128i mask_hi = _mm_set_epi8(0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01, 0, 0, 0, 0, 0, 0, 0, 0);
    __m128i j_lo = _mm_and_si128(ii_lo, mask_lo);
    __m128i j_hi = _mm_and_si128(ii_hi, mask_hi);
    __m128i j = _mm_or_si128(j_lo, j_hi);
    __m128i mask = _mm_set_epi8(0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01);
    return _mm_cmpeq_epi8(j, mask);
}

__m128i hs_simdy_unpack_mask16x8(uint8_t i)
{
    __m128i ii = _mm_set1_epi16(i);
    __m128i mask = _mm_set_epi16(0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01);
    __m128i j = _mm_and_si128(ii, mask);
    return _mm_cmpeq_epi16(j, mask);
}

static const _Alignas(16) int32_t mask32x4_table[4 * 16] = {
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
__m128i hs_simdy_unpack_mask32x4(uint8_t i)
{
    return _mm_load_si128((const __m128i *)&mask32x4_table[(i & 15) * 4]);
}

static const _Alignas(16) int64_t mask64x2_table[8] = {
    0, 0, -1, 0, 0, -1, -1, -1,
};
__m128i hs_simdy_unpack_mask64x2(uint8_t i)
{
    return _mm_load_si128((const __m128i *)&mask64x2_table[(i & 3) * 2]);
}

#endif
