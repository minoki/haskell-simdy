#include "mask.h"

#if defined(__SSE2__)
extern uint16_t hs_simdy_pack_mask8x16(__m128i v);
extern uint8_t hs_simdy_pack_mask16x8(__m128i v);
extern uint8_t hs_simdy_pack_mask32x4(__m128 v);
extern uint8_t hs_simdy_pack_mask64x2(__m128d v);

#if defined(__AVX2__)
extern uint32_t hs_simdy_pack_mask8x32(__m256i v);
extern uint16_t hs_simdy_pack_mask16x16(__m256i v);
extern uint8_t hs_simdy_pack_mask32x8(__m256 v);
extern uint8_t hs_simdy_pack_mask64x4(__m256d v);
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

#elif defined(__aarch64__)

extern uint16_t hs_simdy_pack_mask8x16(uint8x16_t v);
extern uint8_t hs_simdy_pack_mask16x8(uint16x8_t v);
extern uint8_t hs_simdy_pack_mask32x4(uint32x4_t v);
extern uint8_t hs_simdy_pack_mask64x2(uint64x2_t v);

uint8x16_t hs_simdy_mask16x8x2_to_mask8x16(uint16x8_t lo, uint16x8_t hi)
{
    return vcombine_u8(vqmovn_u16(lo), vqmovn_u16(hi));
}

uint16x8_t hs_simdy_mask32x4x2_to_mask16x8(uint32x4_t lo, uint32x4_t hi)
{
    return vcombine_u16(vqmovn_u32(lo), vqmovn_u32(hi));
}

uint32x4_t hs_simdy_mask64x2x2_to_mask32x4(uint64x2_t lo, uint64x2_t hi)
{
    return vcombine_u32(vqmovn_u64(lo), vqmovn_u64(hi));
}

extern uint8x16_t hs_simdy_unpack_mask8x16(uint16_t i);
extern uint16x8_t hs_simdy_unpack_mask16x8(uint8_t i);
const _Alignas(16) uint32_t hs_simdy_mask32x4_table[4 * 16] = {
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
extern uint32x4_t hs_simdy_unpack_mask32x4(uint8_t i);

const _Alignas(16) uint64_t hs_simdy_mask64x2_table[8] = {
    0, 0, -1, 0, 0, -1, -1, -1,
};
extern uint64x2_t hs_simdy_unpack_mask64x2(uint8_t i);

#endif
