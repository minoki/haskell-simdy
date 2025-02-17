#if !defined(SIMDY_MASK_H)
#define SIMDY_MASK_H

#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>

inline __m128i hs_simdy_unpack_mask8x16(uint16_t i)
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

inline __m128i hs_simdy_unpack_mask16x8(uint8_t i)
{
    __m128i ii = _mm_set1_epi16(i);
    __m128i mask = _mm_set_epi16(0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01);
    __m128i j = _mm_and_si128(ii, mask);
    return _mm_cmpeq_epi16(j, mask);
}

extern const _Alignas(16) int32_t hs_simdy_mask32x4_table[4 * 16];
inline __m128i hs_simdy_unpack_mask32x4(uint8_t i)
{
    return _mm_load_si128((const __m128i *)&hs_simdy_mask32x4_table[(i & 15) * 4]);
}

extern const _Alignas(16) int64_t hs_simdy_mask64x2_table[8];
inline __m128i hs_simdy_unpack_mask64x2(uint8_t i)
{
    return _mm_load_si128((const __m128i *)&hs_simdy_mask64x2_table[(i & 3) * 2]);
}

#if defined(__AVX2__)
inline __m256i hs_simdy_unpack_mask8x32(uint32_t i)
{
    __m128i ii0 = _mm_set1_epi8(i & 0xFF);
    __m128i ii1 = _mm_set1_epi8((i >> 8) & 0xFF);
    __m128i ii2 = _mm_set1_epi8((i >> 16) & 0xFF);
    __m128i ii3 = _mm_set1_epi8(i >> 24);
    __m128i mask_lo = _mm_set_epi8(0, 0, 0, 0, 0, 0, 0, 0, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01);
    __m128i mask_hi = _mm_set_epi8(0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01, 0, 0, 0, 0, 0, 0, 0, 0);
    __m128i j0 = _mm_and_si128(ii0, mask_lo);
    __m128i j1 = _mm_and_si128(ii1, mask_hi);
    __m128i j2 = _mm_and_si128(ii2, mask_lo);
    __m128i j3 = _mm_and_si128(ii3, mask_hi);
    __m128i k0 = _mm_or_si128(j0, j1);
    __m128i k1 = _mm_or_si128(j1, j2);
    __m256i k = _mm256_set_m128i(k1, k0);
    __m256i mask = _mm256_set_epi8(0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01);
    return _mm256_cmpeq_epi8(k, mask);
}

inline __m256i hs_simdy_unpack_mask16x16(uint16_t i)
{
    __m256i ii = _mm256_set1_epi16(i);
    __m256i mask = _mm256_set_epi16(0x8000, 0x4000, 0x2000, 0x1000, 0x0800, 0x0400, 0x0200, 0x0100, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01);
    __m256i j = _mm256_and_si256(ii, mask);
    return _mm256_cmpeq_epi16(j, mask);
}

inline __m256i hs_simdy_unpack_mask32x8(uint8_t i)
{
    __m256i ii = _mm256_set1_epi32(i);
    __m256i mask = _mm256_set_epi32(0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01);
    __m256i j = _mm256_and_si256(ii, mask);
    return _mm256_cmpeq_epi32(j, mask);
}

extern const _Alignas(32) int64_t hs_simdy_mask64x4_table[4 * 16];
inline __m256i hs_simdy_unpack_mask64x4(uint8_t i)
{
    return _mm256_load_si256((const __m256i *)&hs_simdy_mask64x4_table[(i & 15) * 4]);
}

#endif

#endif

#endif
