#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>
#include "mask.h"

__m128i hs_simdy_eqInt8X16(__m128i a, __m128i b)
{
    return _mm_cmpeq_epi8(a, b);
}

__m128i hs_simdy_eqInt16X8(__m128i a, __m128i b)
{
    return _mm_cmpeq_epi16(a, b);
}

__m128i hs_simdy_eqInt32X4(__m128i a, __m128i b)
{
    return _mm_cmpeq_epi32(a, b);
}

__m128i hs_simdy_eqInt64X2(__m128i a, __m128i b)
{
    int64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] == bb[1] ? -1 : 0, aa[0] == b[0] ? -1 : 0);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_eqInt64X2_sse41(__m128i a, __m128i b)
{
    return _mm_cmpeq_epi64(a, b);
}

uint16_t hs_simdy_eqInt8X16_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask8x16(hs_simdy_eqInt8X16(a, b));
}

__attribute__((target("avx512vl,avx512bw")))
uint16_t hs_simdy_eqInt8X16_densemask_avx512(__m128i a, __m128i b)
{
    return (uint16_t)_cvtmask16_u32(_mm_cmpeq_epi8_mask(a, b));
}

uint8_t hs_simdy_eqInt16X8_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask16x8(hs_simdy_eqInt16X8(a, b));
}

__attribute__((target("avx512vl,avx512bw,avx512dq")))
uint8_t hs_simdy_eqInt16X8_densemask_avx512(__m128i a, __m128i b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmpeq_epi16_mask(a, b));
}

uint8_t hs_simdy_eqInt32X4_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask32x4(_mm_castsi128_ps(hs_simdy_eqInt32X4(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_eqInt32X4_densemask_avx512(__m128i a, __m128i b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmpeq_epi32_mask(a, b));
}

uint8_t hs_simdy_eqInt64X2_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask64x2(_mm_castsi128_pd(hs_simdy_eqInt64X2(a, b)));
}

__attribute__((target("sse4.1")))
uint8_t hs_simdy_eqInt64X2_densemask_sse41(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask64x2(_mm_castsi128_pd(hs_simdy_eqInt64X2_sse41(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_eqInt64X2_densemask_avx512(__m128i a, __m128i b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmpeq_epi64_mask(a, b));
}

__m128i hs_simdy_ltInt8X16(__m128i a, __m128i b)
{
    return _mm_cmpgt_epi8(b, a);
}

__m128i hs_simdy_ltInt16X8(__m128i a, __m128i b)
{
    return _mm_cmpgt_epi16(b, a);
}

__m128i hs_simdy_ltInt32X4(__m128i a, __m128i b)
{
    return _mm_cmpgt_epi32(b, a);
}

__m128i hs_simdy_ltInt64X2(__m128i a, __m128i b)
{
    int64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] < bb[1] ? -1 : 0, aa[0] < b[0] ? -1 : 0);
}

__attribute__((target("sse4.2")))
__m128i hs_simdy_ltInt64X2_sse42(__m128i a, __m128i b)
{
    return _mm_cmpgt_epi64(b, a);
}

uint16_t hs_simdy_ltInt8X16_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask8x16(hs_simdy_ltInt8X16(a, b));
}

__attribute__((target("avx512vl,avx512bw")))
uint16_t hs_simdy_ltInt8X16_densemask_avx512(__m128i a, __m128i b)
{
    return (uint16_t)_cvtmask16_u32(_mm_cmplt_epi8_mask(a, b));
}

uint8_t hs_simdy_ltInt16X8_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask16x8(hs_simdy_ltInt16X8(a, b));
}

__attribute__((target("avx512vl,avx512bw,avx512dq")))
uint8_t hs_simdy_ltInt16X8_densemask_avx512(__m128i a, __m128i b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epi16_mask(a, b));
}

uint8_t hs_simdy_ltInt32X4_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask32x4(_mm_castsi128_ps(hs_simdy_ltInt32X4(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltInt32X4_densemask_avx512(__m128i a, __m128i b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epi32_mask(a, b));
}

uint8_t hs_simdy_ltInt64X2_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask64x2(_mm_castsi128_pd(hs_simdy_ltInt64X2(a, b)));
}

__attribute__((target("sse4.2")))
uint8_t hs_simdy_ltInt64X2_densemask_sse42(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask64x2(_mm_castsi128_pd(hs_simdy_ltInt64X2_sse42(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltInt64X2_densemask_avx512(__m128i a, __m128i b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epi64_mask(a, b));
}

__m128i hs_simdy_ltWord8X16(__m128i a, __m128i b)
{
    __m128i mask = _mm_set1_epi8(INT8_MIN);
    a = _mm_xor_si128(a, mask);
    b = _mm_xor_si128(b, mask);
    return _mm_cmpgt_epi8(b, a);
}

__m128i hs_simdy_ltWord16X8(__m128i a, __m128i b)
{
    __m128i mask = _mm_set1_epi16(INT16_MIN);
    a = _mm_xor_si128(a, mask);
    b = _mm_xor_si128(b, mask);
    return _mm_cmpgt_epi16(b, a);
}

__m128i hs_simdy_ltWord32X4(__m128i a, __m128i b)
{
    __m128i mask = _mm_set1_epi32(INT32_MIN);
    a = _mm_xor_si128(a, mask);
    b = _mm_xor_si128(b, mask);
    return _mm_cmpgt_epi32(b, a);
}

__m128i hs_simdy_ltWord64X2(__m128i a, __m128i b)
{
    uint64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] < bb[1] ? -1 : 0, aa[0] < b[0] ? -1 : 0);
}

__attribute__((target("sse4.2")))
__m128i hs_simdy_ltWord64X2_sse42(__m128i a, __m128i b)
{
    __m128i mask = _mm_set1_epi64x(INT64_MIN);
    a = _mm_xor_si128(a, mask);
    b = _mm_xor_si128(b, mask);
    return _mm_cmpgt_epi64(b, a);
}

uint16_t hs_simdy_ltWord8X16_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask8x16(hs_simdy_ltWord8X16(a, b));
}

__attribute__((target("avx512vl,avx512bw")))
uint16_t hs_simdy_ltWord8X16_densemask_avx512(__m128i a, __m128i b)
{
    return (uint16_t)_cvtmask16_u32(_mm_cmplt_epu8_mask(a, b));
}

uint8_t hs_simdy_ltWord16X8_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask16x8(hs_simdy_ltWord16X8(a, b));
}

__attribute__((target("avx512vl,avx512bw,avx512dq")))
uint8_t hs_simdy_ltWord16X8_densemask_avx512(__m128i a, __m128i b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epu16_mask(a, b));
}

uint8_t hs_simdy_ltWord32X4_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask32x4(_mm_castsi128_ps(hs_simdy_ltWord32X4(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltWord32X4_densemask_avx512(__m128i a, __m128i b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epu32_mask(a, b));
}

uint8_t hs_simdy_ltWord64X2_densemask(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask64x2(_mm_castsi128_pd(hs_simdy_ltWord64X2(a, b)));
}

__attribute__((target("sse4.2")))
uint8_t hs_simdy_ltWord64X2_densemask_sse42(__m128i a, __m128i b)
{
    return hs_simdy_pack_mask64x2(_mm_castsi128_pd(hs_simdy_ltWord64X2_sse42(a, b)));
}

__attribute__((target("avx512vl,avx512dq")))
uint8_t hs_simdy_ltWord64X2_densemask_avx512(__m128i a, __m128i b)
{
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epu64_mask(a, b));
}

#elif defined(__aarch64__)
#include <stdint.h>
#include <arm_neon.h>
#include "mask.h"

uint8x16_t hs_simdy_eqInt8X16(int8x16_t a, int8x16_t b)
{
    return vceqq_s8(a, b);
}

uint16x8_t hs_simdy_eqInt16X8(int16x8_t a, int16x8_t b)
{
    return vceqq_s16(a, b);
}

uint32x4_t hs_simdy_eqInt32X4(int32x4_t a, int32x4_t b)
{
    return vceqq_s32(a, b);
}

uint64x2_t hs_simdy_eqInt64X2(int64x2_t a, int64x2_t b)
{
    return vceqq_s64(a, b);
}

uint16_t hs_simdy_eqInt8X16_densemask(int8x16_t a, int8x16_t b)
{
    return hs_simdy_pack_mask8x16(hs_simdy_eqInt8X16(a, b));
}

uint8_t hs_simdy_eqInt16X8_densemask(int16x8_t a, int16x8_t b)
{
    return hs_simdy_pack_mask16x8(hs_simdy_eqInt16X8(a, b));
}

uint8_t hs_simdy_eqInt32X4_densemask(int32x4_t a, int32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_eqInt32X4(a, b));
}

uint8_t hs_simdy_eqInt64X2_densemask(int64x2_t a, int64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_eqInt64X2(a, b));
}

uint8x16_t hs_simdy_ltInt8X16(int8x16_t a, int8x16_t b)
{
    return vcltq_s8(a, b);
}

uint16x8_t hs_simdy_ltInt16X8(int16x8_t a, int16x8_t b)
{
    return vcltq_s16(a, b);
}

uint32x4_t hs_simdy_ltInt32X4(int32x4_t a, int32x4_t b)
{
    return vcltq_s32(a, b);
}

uint64x2_t hs_simdy_ltInt64X2(int64x2_t a, int64x2_t b)
{
    return vcltq_s64(a, b);
}

uint16_t hs_simdy_ltInt8X16_densemask(int8x16_t a, int8x16_t b)
{
    return hs_simdy_pack_mask8x16(hs_simdy_ltInt8X16(a, b));
}

uint8_t hs_simdy_ltInt16X8_densemask(int16x8_t a, int16x8_t b)
{
    return hs_simdy_pack_mask16x8(hs_simdy_ltInt16X8(a, b));
}

uint8_t hs_simdy_ltInt32X4_densemask(int32x4_t a, int32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_ltInt32X4(a, b));
}

uint8_t hs_simdy_ltInt64X2_densemask(int64x2_t a, int64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_ltInt64X2(a, b));
}

uint8x16_t hs_simdy_ltWord8X16(uint8x16_t a, uint8x16_t b)
{
    return vcltq_u8(a, b);
}

uint16x8_t hs_simdy_ltWord16X8(uint16x8_t a, uint16x8_t b)
{
    return vcltq_u16(a, b);
}

uint32x4_t hs_simdy_ltWord32X4(uint32x4_t a, uint32x4_t b)
{
    return vcltq_u32(a, b);
}

uint64x2_t hs_simdy_ltWord64X2(uint64x2_t a, uint64x2_t b)
{
    return vcltq_u64(a, b);
}

uint16_t hs_simdy_ltWord8X16_densemask(uint8x16_t a, uint8x16_t b)
{
    return hs_simdy_pack_mask8x16(hs_simdy_ltWord8X16(a, b));
}

uint8_t hs_simdy_ltWord16X8_densemask(uint16x8_t a, uint16x8_t b)
{
    return hs_simdy_pack_mask16x8(hs_simdy_ltWord16X8(a, b));
}

uint8_t hs_simdy_ltWord32X4_densemask(uint32x4_t a, uint32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_ltWord32X4(a, b));
}

uint8_t hs_simdy_ltWord64X2_densemask(uint64x2_t a, uint64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_ltWord64X2(a, b));
}

#endif
