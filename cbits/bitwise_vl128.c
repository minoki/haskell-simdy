#if defined(__SSE2__)
#include <stdint.h>
#include <string.h>
#include <emmintrin.h>
#include <immintrin.h> // _mm_sra_epi64
#include "HsFFI.h"

__m128i hs_simdy_complementInt8X16(__m128i x)
{
    __m128i y = _mm_set1_epi32(-1);
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_complementInt16X8(__m128i x)
{
    __m128i y = _mm_set1_epi32(-1);
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_complementInt32X4(__m128i x)
{
    __m128i y = _mm_set1_epi32(-1);
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_complementInt64X2(__m128i x)
{
    __m128i y = _mm_set1_epi32(-1);
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_complementWord8X16(__m128i x)
{
    __m128i y = _mm_set1_epi32(-1);
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_complementWord16X8(__m128i x)
{
    __m128i y = _mm_set1_epi32(-1);
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_complementWord32X4(__m128i x)
{
    __m128i y = _mm_set1_epi32(-1);
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_complementWord64X2(__m128i x)
{
    __m128i y = _mm_set1_epi32(-1);
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_andInt8X16(__m128i x, __m128i y)
{
    return _mm_and_si128(x, y);
}

__m128i hs_simdy_andInt16X8(__m128i x, __m128i y)
{
    return _mm_and_si128(x, y);
}

__m128i hs_simdy_andInt32X4(__m128i x, __m128i y)
{
    return _mm_and_si128(x, y);
}

__m128i hs_simdy_andInt64X2(__m128i x, __m128i y)
{
    return _mm_and_si128(x, y);
}

__m128i hs_simdy_andWord8X16(__m128i x, __m128i y)
{
    return _mm_and_si128(x, y);
}

__m128i hs_simdy_andWord16X8(__m128i x, __m128i y)
{
    return _mm_and_si128(x, y);
}

__m128i hs_simdy_andWord32X4(__m128i x, __m128i y)
{
    return _mm_and_si128(x, y);
}

__m128i hs_simdy_andWord64X2(__m128i x, __m128i y)
{
    return _mm_and_si128(x, y);
}

__m128i hs_simdy_orInt8X16(__m128i x, __m128i y)
{
    return _mm_or_si128(x, y);
}

__m128i hs_simdy_orInt16X8(__m128i x, __m128i y)
{
    return _mm_or_si128(x, y);
}

__m128i hs_simdy_orInt32X4(__m128i x, __m128i y)
{
    return _mm_or_si128(x, y);
}

__m128i hs_simdy_orInt64X2(__m128i x, __m128i y)
{
    return _mm_or_si128(x, y);
}

__m128i hs_simdy_orWord8X16(__m128i x, __m128i y)
{
    return _mm_or_si128(x, y);
}

__m128i hs_simdy_orWord16X8(__m128i x, __m128i y)
{
    return _mm_or_si128(x, y);
}

__m128i hs_simdy_orWord32X4(__m128i x, __m128i y)
{
    return _mm_or_si128(x, y);
}

__m128i hs_simdy_orWord64X2(__m128i x, __m128i y)
{
    return _mm_or_si128(x, y);
}

__m128i hs_simdy_xorInt8X16(__m128i x, __m128i y)
{
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_xorInt16X8(__m128i x, __m128i y)
{
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_xorInt32X4(__m128i x, __m128i y)
{
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_xorInt64X2(__m128i x, __m128i y)
{
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_xorWord8X16(__m128i x, __m128i y)
{
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_xorWord16X8(__m128i x, __m128i y)
{
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_xorWord32X4(__m128i x, __m128i y)
{
    return _mm_xor_si128(x, y);
}

__m128i hs_simdy_xorWord64X2(__m128i x, __m128i y)
{
    return _mm_xor_si128(x, y);
}

//
// shiftL
//

__m128i hs_simdy_shiftLInt8x16(__m128i x, HsInt y)
{
    __m128i zero = _mm_setzero_si128();
    __m128i x_lo = _mm_unpacklo_epi8(x, zero);
    __m128i x_hi = _mm_unpackhi_epi8(x, zero);
    __m128i count = _mm_set_epi64x(0, y);
    __m128i mask = _mm_set1_epi16(0xff);
    x_lo = _mm_sll_epi16(x_lo, count);
    x_lo = _mm_and_si128(x_lo, mask);
    x_hi = _mm_sll_epi16(x_hi, count);
    x_hi = _mm_and_si128(x_hi, mask);
    return _mm_packus_epi16(x_lo, x_hi);
}

__m128i hs_simdy_shiftLInt16x8(__m128i x, HsInt y)
{
    return _mm_sll_epi16(x, _mm_set_epi64x(0, y));
}

__m128i hs_simdy_shiftLInt32x4(__m128i x, HsInt y)
{
    return _mm_sll_epi32(x, _mm_set_epi64x(0, y));
}

__m128i hs_simdy_shiftLInt64x2(__m128i x, HsInt y)
{
    return _mm_sll_epi64(x, _mm_set_epi64x(0, y));
}

__m128i hs_simdy_shiftLWord8x16(__m128i x, HsInt y)
{
    return hs_simdy_shiftLInt8x16(x, y);
}

__m128i hs_simdy_shiftLWord16x8(__m128i x, HsInt y)
{
    return _mm_sll_epi16(x, _mm_set_epi64x(0, y));
}

__m128i hs_simdy_shiftLWord32x4(__m128i x, HsInt y)
{
    return _mm_sll_epi32(x, _mm_set_epi64x(0, y));
}

__m128i hs_simdy_shiftLWord64x2(__m128i x, HsInt y)
{
    return _mm_sll_epi64(x, _mm_set_epi64x(0, y));
}

//
// shiftR
//

__m128i hs_simdy_shiftRInt8x16(__m128i x, HsInt y)
{
    __m128i zero = _mm_setzero_si128();
    __m128i x_lo = _mm_unpacklo_epi8(zero, x);
    __m128i x_hi = _mm_unpackhi_epi8(zero, x);
    __m128i count = _mm_set_epi64x(0, y + 8);
    __m128i mask = _mm_set1_epi16(0xff);
    x_lo = _mm_sra_epi16(x_lo, count);
    x_lo = _mm_and_si128(x_lo, mask);
    x_hi = _mm_sra_epi16(x_hi, count);
    x_hi = _mm_and_si128(x_hi, mask);
    return _mm_packus_epi16(x_lo, x_hi);
}

__m128i hs_simdy_shiftRInt16x8(__m128i x, HsInt y)
{
    return _mm_sra_epi16(x, _mm_set_epi64x(0, y));
}

__m128i hs_simdy_shiftRInt32x4(__m128i x, HsInt y)
{
    return _mm_sra_epi32(x, _mm_set_epi64x(0, y));
}

__m128i hs_simdy_shiftRInt64x2(__m128i x, HsInt y)
{
#if defined(__AVX512F__) && defined(__AVX512VL__)
    return _mm_sra_epi64(x, _mm_set_epi64x(0, y));
#else
    _Alignas(16) int64_t buf[2];
    memcpy(buf, &x, 16);
    if (y >= 64) {
        y = 63;
    }
    buf[0] >>= y;
    buf[1] >>= y;
    memcpy(&x, buf, 16);
    return x;
#endif
}

__m128i hs_simdy_shiftRWord8x16(__m128i x, HsInt y)
{
    __m128i zero = _mm_setzero_si128();
    __m128i x_lo = _mm_unpacklo_epi8(x, zero);
    __m128i x_hi = _mm_unpackhi_epi8(x, zero);
    __m128i count = _mm_set_epi64x(0, y);
    __m128i mask = _mm_set1_epi16(0xff);
    x_lo = _mm_srl_epi16(x_lo, count);
    x_lo = _mm_and_si128(x_lo, mask);
    x_hi = _mm_srl_epi16(x_hi, count);
    x_hi = _mm_and_si128(x_hi, mask);
    return _mm_packus_epi16(x_lo, x_hi);
}

__m128i hs_simdy_shiftRWord16x8(__m128i x, HsInt y)
{
    return _mm_srl_epi16(x, _mm_set_epi64x(0, y));
}

__m128i hs_simdy_shiftRWord32x4(__m128i x, HsInt y)
{
    return _mm_srl_epi32(x, _mm_set_epi64x(0, y));
}

__m128i hs_simdy_shiftRWord64x2(__m128i x, HsInt y)
{
    return _mm_srl_epi64(x, _mm_set_epi64x(0, y));
}

#elif defined(__aarch64__)
#include <stdint.h>
#include <arm_neon.h>
#include "HsFFI.h"

//
// complement
//

int8x16_t hs_simdy_complementInt8x16(int8x16_t x)
{
    return vmvnq_s8(x);
}

int16x8_t hs_simdy_complementInt16x8(int16x8_t x)
{
    return vmvnq_s16(x);
}

int32x4_t hs_simdy_complementInt32x4(int32x4_t x)
{
    return vmvnq_s32(x);
}

int64x2_t hs_simdy_complementInt64x2(int64x2_t x)
{
    return vreinterpretq_s64_s32(vmvnq_s32(vreinterpretq_s32_s64(x)));
}

uint8x16_t hs_simdy_complementWord8x16(uint8x16_t x)
{
    return vmvnq_u8(x);
}

uint16x8_t hs_simdy_complementWord16x8(uint16x8_t x)
{
    return vmvnq_u16(x);
}

uint32x4_t hs_simdy_complementWord32x4(uint32x4_t x)
{
    return vmvnq_u32(x);
}

uint64x2_t hs_simdy_complementWord64x2(uint64x2_t x)
{
    return vreinterpretq_u64_u32(vmvnq_u32(vreinterpretq_u32_u64(x)));
}

//
// and
//

int8x16_t hs_simdy_andInt8x16(int8x16_t x, int8x16_t y)
{
    return vandq_s8(x, y);
}

int16x8_t hs_simdy_andInt16x8(int16x8_t x, int16x8_t y)
{
    return vandq_s16(x, y);
}

int32x4_t hs_simdy_andInt32x4(int32x4_t x, int32x4_t y)
{
    return vandq_s32(x, y);
}

int64x2_t hs_simdy_andInt64x2(int64x2_t x, int64x2_t y)
{
    return vandq_s64(x, y);
}

uint8x16_t hs_simdy_andWord8x16(uint8x16_t x, uint8x16_t y)
{
    return vandq_u8(x, y);
}

uint16x8_t hs_simdy_andWord16x8(uint16x8_t x, uint16x8_t y)
{
    return vandq_u16(x, y);
}

uint32x4_t hs_simdy_andWord32x4(uint32x4_t x, uint32x4_t y)
{
    return vandq_u32(x, y);
}

uint64x2_t hs_simdy_andWord64x2(uint64x2_t x, uint64x2_t y)
{
    return vandq_u64(x, y);
}

//
// or
//

int8x16_t hs_simdy_orInt8x16(int8x16_t x, int8x16_t y)
{
    return vorrq_s8(x, y);
}

int16x8_t hs_simdy_orInt16x8(int16x8_t x, int16x8_t y)
{
    return vorrq_s16(x, y);
}

int32x4_t hs_simdy_orInt32x4(int32x4_t x, int32x4_t y)
{
    return vorrq_s32(x, y);
}

int64x2_t hs_simdy_orInt64x2(int64x2_t x, int64x2_t y)
{
    return vorrq_s64(x, y);
}

uint8x16_t hs_simdy_orWord8x16(uint8x16_t x, uint8x16_t y)
{
    return vorrq_u8(x, y);
}

uint16x8_t hs_simdy_orWord16x8(uint16x8_t x, uint16x8_t y)
{
    return vorrq_u16(x, y);
}

uint32x4_t hs_simdy_orWord32x4(uint32x4_t x, uint32x4_t y)
{
    return vorrq_u32(x, y);
}

uint64x2_t hs_simdy_orWord64x2(uint64x2_t x, uint64x2_t y)
{
    return vorrq_u64(x, y);
}

//
// xor
//

int8x16_t hs_simdy_xorInt8x16(int8x16_t x, int8x16_t y)
{
    return veorq_s8(x, y);
}

int16x8_t hs_simdy_xorInt16x8(int16x8_t x, int16x8_t y)
{
    return veorq_s16(x, y);
}

int32x4_t hs_simdy_xorInt32x4(int32x4_t x, int32x4_t y)
{
    return veorq_s32(x, y);
}

int64x2_t hs_simdy_xorInt64x2(int64x2_t x, int64x2_t y)
{
    return veorq_s64(x, y);
}

uint8x16_t hs_simdy_xorWord8x16(uint8x16_t x, uint8x16_t y)
{
    return veorq_u8(x, y);
}

uint16x8_t hs_simdy_xorWord16x8(uint16x8_t x, uint16x8_t y)
{
    return veorq_u16(x, y);
}

uint32x4_t hs_simdy_xorWord32x4(uint32x4_t x, uint32x4_t y)
{
    return veorq_u32(x, y);
}

uint64x2_t hs_simdy_xorWord64x2(uint64x2_t x, uint64x2_t y)
{
    return veorq_u64(x, y);
}

//
// shiftL
//

int8x16_t hs_simdy_shiftLInt8x16(int8x16_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_s8(x, vdupq_n_s8(y));
}

int16x8_t hs_simdy_shiftLInt16x8(int16x8_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_s16(x, vdupq_n_s16(y));
}

int32x4_t hs_simdy_shiftLInt32x4(int32x4_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_s32(x, vdupq_n_s32(y));
}

int64x2_t hs_simdy_shiftLInt64x2(int64x2_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_s64(x, vdupq_n_s64(y));
}

uint8x16_t hs_simdy_shiftLWord8x16(uint8x16_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_u8(x, vdupq_n_s8(y));
}

uint16x8_t hs_simdy_shiftLWord16x8(uint16x8_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_u16(x, vdupq_n_s16(y));
}

uint32x4_t hs_simdy_shiftLWord32x4(uint32x4_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_u32(x, vdupq_n_s32(y));
}

uint64x2_t hs_simdy_shiftLWord64x2(uint64x2_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_u64(x, vdupq_n_s64(y));
}

//
// shiftR
//

int8x16_t hs_simdy_shiftRInt8x16(int8x16_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_s8(x, vdupq_n_s8(-y));
}

int16x8_t hs_simdy_shiftRInt16x8(int16x8_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_s16(x, vdupq_n_s16(-y));
}

int32x4_t hs_simdy_shiftRInt32x4(int32x4_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_s32(x, vdupq_n_s32(-y));
}

int64x2_t hs_simdy_shiftRInt64x2(int64x2_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_s64(x, vdupq_n_s64(-y));
}

uint8x16_t hs_simdy_shiftRWord8x16(uint8x16_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_u8(x, vdupq_n_s8(-y));
}

uint16x8_t hs_simdy_shiftRWord16x8(uint16x8_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_u16(x, vdupq_n_s16(-y));
}

uint32x4_t hs_simdy_shiftRWord32x4(uint32x4_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_u32(x, vdupq_n_s32(-y));
}

uint64x2_t hs_simdy_shiftRWord64x2(uint64x2_t x, HsInt y)
{
    if (y >= 128) y = 127;
    return vshlq_u64(x, vdupq_n_s64(-y));
}

#endif
