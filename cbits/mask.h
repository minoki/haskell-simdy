#if !defined(SIMDY_MASK_H)
#define SIMDY_MASK_H

#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>

inline uint16_t hs_simdy_pack_mask8x16(__m128i v)
{
    return (uint16_t)_mm_movemask_epi8(v);
}

inline uint8_t hs_simdy_pack_mask16x8(__m128i v)
{
    __m128i zero = _mm_set1_epi16(0);
    __m128i mask8x16 = _mm_packs_epi16(v, zero);
    return (uint8_t)_mm_movemask_epi8(mask8x16);
}

inline uint8_t hs_simdy_pack_mask32x4(__m128 v)
{
    return (uint8_t)_mm_movemask_ps(v);
}

inline uint8_t hs_simdy_pack_mask64x2(__m128d v)
{
    return (uint8_t)_mm_movemask_pd(v);
}

#if defined(__AVX2__)
inline uint32_t hs_simdy_pack_mask8x32(__m256i v)
{
    return (uint32_t)_mm256_movemask_epi8(v);
}

inline uint16_t hs_simdy_pack_mask16x16(__m256i v)
{
    __m256i zero = _mm256_setzero_si256();
    __m256i mask8x32 = _mm256_packs_epi16(v, zero);
    return (uint16_t)_mm256_movemask_epi8(mask8x32);
}

inline uint8_t hs_simdy_pack_mask32x8(__m256 v)
{
    return (uint8_t)_mm256_movemask_ps(v);
}

inline uint8_t hs_simdy_pack_mask64x4(__m256d v)
{
    return (uint8_t)_mm256_movemask_pd(v);
}
#endif

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

#elif defined(__aarch64__)
#include <stdint.h>
#include <arm_neon.h>

inline uint16_t hs_simdy_pack_mask8x16(uint8x16_t v)
{
    static const uint8_t mask_array[16] = {0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80};
    uint8x16_t mask = vld1q_u8(mask_array);
    uint8x16_t masked = vandq_u8(v, mask);
    uint8x16_t masked_hi = vextq_u8(masked, masked, 8); // {masked[8], ..., masked[15]}
    uint8x16_t zipped = vzip1q_u8(masked, masked_hi); // {masked[0], masked_hi[0], masked[1], masked_hi[1], ...}
    uint16x8_t b = vreinterpretq_u16_u8(zipped);
    return vaddvq_u16(b);
}

inline uint8_t hs_simdy_pack_mask16x8(uint16x8_t v)
{
    static const uint16_t mask_array[8] = {0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80};
    uint16x8_t mask = vld1q_u16(mask_array);
    uint16x8_t masked = vandq_u16(v, mask);
    return vaddvq_u16(masked);
}

inline uint8_t hs_simdy_pack_mask32x4(uint32x4_t v)
{
    static const uint32_t mask_array[4] = {0x01, 0x02, 0x04, 0x08};
    uint32x4_t mask = vld1q_u32(mask_array);
    uint32x4_t masked = vandq_u32(v, mask);
    return vaddvq_u32(masked);
}

inline uint8_t hs_simdy_pack_mask64x2(uint64x2_t v)
{
    static const uint64_t mask_array[2] = {0x01, 0x02};
    uint64x2_t mask = vld1q_u64(mask_array);
    uint64x2_t masked = vandq_u64(v, mask);
    return vaddvq_u64(masked);
}

inline uint8x16_t hs_simdy_unpack_mask8x16(uint16_t i)
{
    uint8x16_t ii_lo = vdupq_n_u8(i & 0xFF);
    uint8x16_t ii_hi = vdupq_n_u8(i >> 8);
    static const uint8_t mask_lo_array[16] = {0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0, 0, 0, 0, 0, 0, 0, 0};
    uint8x16_t mask_lo = vld1q_u8(mask_lo_array);
    static const uint8_t mask_hi_array[16] = {0, 0, 0, 0, 0, 0, 0, 0, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80};
    uint8x16_t mask_hi = vld1q_u8(mask_hi_array);
    uint8x16_t j_lo = vandq_u8(ii_lo, mask_lo);
    uint8x16_t j_hi = vandq_u8(ii_hi, mask_hi);
    uint8x16_t j = vorrq_u8(j_lo, j_hi);
    static const uint8_t mask_array[16] = {0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80};
    uint8x16_t mask = vld1q_u8(mask_array);
    return vceqq_u8(j, mask);
}

inline uint16x8_t hs_simdy_unpack_mask16x8(uint8_t i)
{
    uint16x8_t ii = vdupq_n_u16(i);
    static const uint16_t mask_array[16] = {0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80};
    uint16x8_t mask = vld1q_u16(mask_array);
    uint16x8_t j = vandq_u16(ii, mask);
    return vceqq_u16(j, mask);
}

extern const _Alignas(16) uint32_t hs_simdy_mask32x4_table[4 * 16];
inline uint32x4_t hs_simdy_unpack_mask32x4(uint8_t i)
{
    return vld1q_u32(&hs_simdy_mask32x4_table[(i & 15) * 4]);
}

extern const _Alignas(16) uint64_t hs_simdy_mask64x2_table[8];
inline uint64x2_t hs_simdy_unpack_mask64x2(uint8_t i)
{
    return vld1q_u64(&hs_simdy_mask64x2_table[(i & 3) * 2]);
}

#endif // SSE2 / AArch64

#endif // include guard
