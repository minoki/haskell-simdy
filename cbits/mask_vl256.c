#include "mask.h"

#if defined(__AVX2__)
extern uint32_t hs_simdy_pack_mask8x32(__m256i v);
extern uint16_t hs_simdy_pack_mask16x16(__m256i v);
extern uint8_t hs_simdy_pack_mask32x8(__m256 v);
extern uint8_t hs_simdy_pack_mask64x4(__m256d v);

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
