#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <smmintrin.h>
#include "mask.h"

__m128i hs_simdy_int8x16_eq(__m128i a, __m128i b)
{
    return _mm_cmpeq_epi8(a, b);
}

__m128i hs_simdy_int16x8_eq(__m128i a, __m128i b)
{
    return _mm_cmpeq_epi16(a, b);
}

__m128i hs_simdy_int32x4_eq(__m128i a, __m128i b)
{
    return _mm_cmpeq_epi32(a, b);
}

#if defined(__SSE4_1__)
__m128i hs_simdy_int64x2_eq(__m128i a, __m128i b)
{
    return _mm_cmpeq_epi64(a, b);
}
#else
__m128i hs_simdy_int64x2_eq(__m128i a, __m128i b)
{
    int64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] == bb[1] ? -1 : 0, aa[0] == b[0] ? -1 : 0);
}
#endif

#if defined(__AVX2__)
__m256i hs_simdy_int8x32_eq(__m256i a, __m256i b)
{
    return _mm256_cmpeq_epi8(a, b);
}

__m256i hs_simdy_int16x16_eq(__m256i a, __m256i b)
{
    return _mm256_cmpeq_epi16(a, b);
}

__m256i hs_simdy_int32x8_eq(__m256i a, __m256i b)
{
    return _mm256_cmpeq_epi32(a, b);
}

__m256i hs_simdy_int64x4_eq(__m256i a, __m256i b)
{
    return _mm256_cmpeq_epi64(a, b);
}
#endif

uint16_t hs_simdy_int8x16_eq_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint16_t)_cvtmask16_u32(_mm_cmpeq_epi8_mask(a, b));
#else
    return hs_simdy_pack_mask8x16(hs_simdy_int8x16_eq(a, b));
#endif
}

uint8_t hs_simdy_int16x8_eq_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm_cmpeq_epi16_mask(a, b));
#else
    return hs_simdy_pack_mask16x8(hs_simdy_int16x8_eq(a, b));
#endif
}

uint8_t hs_simdy_int32x4_eq_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm_cmpeq_epi32_mask(a, b));
#else
    return hs_simdy_pack_mask32x4(_mm_castsi128_ps(hs_simdy_int32x4_eq(a, b)));
#endif
}

uint8_t hs_simdy_int64x2_eq_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm_cmpeq_epi64_mask(a, b));
#else
    return hs_simdy_pack_mask64x2(_mm_castsi128_pd(hs_simdy_int64x2_eq(a, b)));
#endif
}

#if defined(__AVX2__)
uint32_t hs_simdy_int8x32_eq_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512BW__) && defined(__AVX512VL__)
    return _cvtmask32_u32(_mm256_cmpeq_epi8_mask(a, b));
#else
    return hs_simdy_pack_mask8x32(hs_simdy_int8x32_eq(a, b));
#endif
}

uint16_t hs_simdy_int16x16_eq_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512BW__) && defined(__AVX512VL__)
    return (uint16_t)_cvtmask16_u32(_mm256_cmpeq_epi16_mask(a, b));
#else
    return hs_simdy_pack_mask16x16(hs_simdy_int16x16_eq(a, b));
#endif
}

uint8_t hs_simdy_int32x8_eq_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmpeq_epi32_mask(a, b));
#else
    return hs_simdy_pack_mask32x8(_mm256_castsi256_ps(hs_simdy_int32x8_eq(a, b)));
#endif
}

uint8_t hs_simdy_int64x4_eq_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmpeq_epi64_mask(a, b));
#else
    return hs_simdy_pack_mask64x4(_mm256_castsi256_pd(hs_simdy_int64x4_eq(a, b)));
#endif
}
#endif

#if defined(__AVX512F__) && defined(__AVX512BW__)
uint64_t hs_simdy_int8x64_eq_densemask(__m512i a, __m512i b)
{
    return _cvtmask64_u64(_mm512_cmpeq_epi8_mask(a, b));
}

uint32_t hs_simdy_int16x32_eq_densemask(__m512i a, __m512i b)
{
    return _cvtmask32_u32(_mm512_cmpeq_epi16_mask(a, b));
}

uint16_t hs_simdy_int32x16_eq_densemask(__m512i a, __m512i b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmpeq_epi32_mask(a, b));
}

uint8_t hs_simdy_int64x8_eq_densemask(__m512i a, __m512i b)
{
    return (uint8_t)_cvtmask8_u32(_mm512_cmpeq_epi64_mask(a, b));
}
#endif

__m128i hs_simdy_int8x16_lt(__m128i a, __m128i b)
{
    return _mm_cmpgt_epi8(b, a);
}

__m128i hs_simdy_int16x8_lt(__m128i a, __m128i b)
{
    return _mm_cmpgt_epi16(b, a);
}

__m128i hs_simdy_int32x4_lt(__m128i a, __m128i b)
{
    return _mm_cmpgt_epi32(b, a);
}

#if defined(__SSE4_2__)
__m128i hs_simdy_int64x2_lt(__m128i a, __m128i b)
{
    return _mm_cmpgt_epi64(b, a);
}
#else
__m128i hs_simdy_int64x2_lt(__m128i a, __m128i b)
{
    int64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] < bb[1] ? -1 : 0, aa[0] < b[0] ? -1 : 0);
}
#endif

#if defined(__AVX2__)
__m256i hs_simdy_int8x32_lt(__m256i a, __m256i b)
{
    return _mm256_cmpgt_epi8(b, a);
}

__m256i hs_simdy_int16x16_lt(__m256i a, __m256i b)
{
    return _mm256_cmpgt_epi16(b, a);
}

__m256i hs_simdy_int32x8_lt(__m256i a, __m256i b)
{
    return _mm256_cmpgt_epi32(b, a);
}

__m256i hs_simdy_int64x4_lt(__m256i a, __m256i b)
{
    return _mm256_cmpgt_epi64(b, a);
}
#endif

uint16_t hs_simdy_int8x16_lt_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint16_t)_cvtmask16_u32(_mm_cmplt_epi8_mask(a, b));
#else
    return hs_simdy_pack_mask8x16(hs_simdy_int8x16_lt(a, b));
#endif
}

uint8_t hs_simdy_int16x8_lt_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epi16_mask(a, b));
#else
    return hs_simdy_pack_mask16x8(hs_simdy_int16x8_lt(a, b));
#endif
}

uint8_t hs_simdy_int32x4_lt_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epi32_mask(a, b));
#else
    return hs_simdy_pack_mask32x4(_mm_castsi128_ps(hs_simdy_int32x4_lt(a, b)));
#endif
}

uint8_t hs_simdy_int64x2_lt_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epi64_mask(a, b));
#else
    return hs_simdy_pack_mask64x2(_mm_castsi128_pd(hs_simdy_int64x2_lt(a, b)));
#endif
}

#if defined(__AVX2__)
uint32_t hs_simdy_int8x32_lt_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512BW__) && defined(__AVX512VL__)
    return _cvtmask32_u32(_mm256_cmplt_epi8_mask(a, b));
#else
    return hs_simdy_pack_mask8x32(hs_simdy_int8x32_lt(a, b));
#endif
}

uint16_t hs_simdy_int16x16_lt_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512BW__) && defined(__AVX512VL__)
    return (uint16_t)_cvtmask16_u32(_mm256_cmplt_epi16_mask(a, b));
#else
    return hs_simdy_pack_mask16x16(hs_simdy_int16x16_lt(a, b));
#endif
}

uint8_t hs_simdy_int32x8_lt_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmplt_epi32_mask(a, b));
#else
    return hs_simdy_pack_mask32x8(_mm256_castsi256_ps(hs_simdy_int32x8_lt(a, b)));
#endif
}

uint8_t hs_simdy_int64x4_lt_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmplt_epi64_mask(a, b));
#else
    return hs_simdy_pack_mask64x4(_mm256_castsi256_pd(hs_simdy_int64x4_lt(a, b)));
#endif
}
#endif

#if defined(__AVX512F__) && defined(__AVX512BW__)
uint64_t hs_simdy_int8x64_lt_densemask(__m512i a, __m512i b)
{
    return _cvtmask64_u64(_mm512_cmplt_epi8_mask(a, b));
}

uint32_t hs_simdy_int16x32_lt_densemask(__m512i a, __m512i b)
{
    return _cvtmask32_u32(_mm512_cmplt_epi16_mask(a, b));
}

uint16_t hs_simdy_int32x16_lt_densemask(__m512i a, __m512i b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmplt_epi32_mask(a, b));
}

uint8_t hs_simdy_int64x8_lt_densemask(__m512i a, __m512i b)
{
    return (uint8_t)_cvtmask8_u32(_mm512_cmplt_epi64_mask(a, b));
}
#endif

__m128i hs_simdy_word8x16_lt(__m128i a, __m128i b)
{
    __m128i mask = _mm_set1_epi8(INT8_MIN);
    a = _mm_xor_si128(a, mask);
    b = _mm_xor_si128(b, mask);
    return _mm_cmpgt_epi8(b, a);
}

__m128i hs_simdy_word16x8_lt(__m128i a, __m128i b)
{
    __m128i mask = _mm_set1_epi16(INT16_MIN);
    a = _mm_xor_si128(a, mask);
    b = _mm_xor_si128(b, mask);
    return _mm_cmpgt_epi16(b, a);
}

__m128i hs_simdy_word32x4_lt(__m128i a, __m128i b)
{
    __m128i mask = _mm_set1_epi32(INT32_MIN);
    a = _mm_xor_si128(a, mask);
    b = _mm_xor_si128(b, mask);
    return _mm_cmpgt_epi32(b, a);
}

#if defined(__SSE4_2__)
__m128i hs_simdy_word64x2_lt(__m128i a, __m128i b)
{
    __m128i mask = _mm_set1_epi64x(INT64_MIN);
    a = _mm_xor_si128(a, mask);
    b = _mm_xor_si128(b, mask);
    return _mm_cmpgt_epi64(b, a);
}
#else
__m128i hs_simdy_word64x2_lt(__m128i a, __m128i b)
{
    uint64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] < bb[1] ? -1 : 0, aa[0] < b[0] ? -1 : 0);
}
#endif

#if defined(__AVX2__)
__m256i hs_simdy_word8x32_lt(__m256i a, __m256i b)
{
    __m256i mask = _mm256_set1_epi8(INT8_MIN);
    a = _mm256_xor_si256(a, mask);
    b = _mm256_xor_si256(b, mask);
    return _mm256_cmpgt_epi8(b, a);
}

__m256i hs_simdy_word16x16_lt(__m256i a, __m256i b)
{
    __m256i mask = _mm256_set1_epi16(INT16_MIN);
    a = _mm256_xor_si256(a, mask);
    b = _mm256_xor_si256(b, mask);
    return _mm256_cmpgt_epi16(b, a);
}

__m256i hs_simdy_word32x8_lt(__m256i a, __m256i b)
{
    __m256i mask = _mm256_set1_epi32(INT32_MIN);
    a = _mm256_xor_si256(a, mask);
    b = _mm256_xor_si256(b, mask);
    return _mm256_cmpgt_epi32(b, a);
}

__m256i hs_simdy_word64x4_lt(__m256i a, __m256i b)
{
    __m256i mask = _mm256_set1_epi64x(INT64_MIN);
    a = _mm256_xor_si256(a, mask);
    b = _mm256_xor_si256(b, mask);
    return _mm256_cmpgt_epi64(b, a);
}
#endif

uint16_t hs_simdy_word8x16_lt_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint16_t)_cvtmask16_u32(_mm_cmplt_epu8_mask(a, b));
#else
    return hs_simdy_pack_mask8x16(hs_simdy_word8x16_lt(a, b));
#endif
}

uint8_t hs_simdy_word16x8_lt_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epu16_mask(a, b));
#else
    return hs_simdy_pack_mask16x8(hs_simdy_word16x8_lt(a, b));
#endif
}

uint8_t hs_simdy_word32x4_lt_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epu32_mask(a, b));
#else
    return hs_simdy_pack_mask32x4(_mm_castsi128_ps(hs_simdy_word32x4_lt(a, b)));
#endif
}

uint8_t hs_simdy_word64x2_lt_densemask(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm_cmplt_epu64_mask(a, b));
#else
    return hs_simdy_pack_mask64x2(_mm_castsi128_pd(hs_simdy_word64x2_lt(a, b)));
#endif
}

#if defined(__AVX2__)
uint32_t hs_simdy_word8x32_lt_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512BW__) && defined(__AVX512VL__)
    return _cvtmask32_u32(_mm256_cmplt_epu8_mask(a, b));
#else
    return hs_simdy_pack_mask8x32(hs_simdy_word8x32_lt(a, b));
#endif
}

uint16_t hs_simdy_word16x16_lt_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512BW__) && defined(__AVX512VL__)
    return (uint16_t)_cvtmask16_u32(_mm256_cmplt_epu16_mask(a, b));
#else
    return hs_simdy_pack_mask16x16(hs_simdy_word16x16_lt(a, b));
#endif
}

uint8_t hs_simdy_word32x8_lt_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmplt_epu32_mask(a, b));
#else
    return hs_simdy_pack_mask32x8(_mm256_castsi256_ps(hs_simdy_word32x8_lt(a, b)));
#endif
}

uint8_t hs_simdy_word64x4_lt_densemask(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return (uint8_t)_cvtmask8_u32(_mm256_cmplt_epu64_mask(a, b));
#else
    return hs_simdy_pack_mask64x4(_mm256_castsi256_pd(hs_simdy_word64x4_lt(a, b)));
#endif
}
#endif

#if defined(__AVX512F__) && defined(__AVX512BW__)
uint64_t hs_simdy_word8x64_lt_densemask(__m512i a, __m512i b)
{
    return _cvtmask64_u64(_mm512_cmplt_epu8_mask(a, b));
}

uint32_t hs_simdy_word16x32_lt_densemask(__m512i a, __m512i b)
{
    return _cvtmask32_u32(_mm512_cmplt_epu16_mask(a, b));
}

uint16_t hs_simdy_word32x16_lt_densemask(__m512i a, __m512i b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmplt_epu32_mask(a, b));
}

uint8_t hs_simdy_word64x8_lt_densemask(__m512i a, __m512i b)
{
    return (uint8_t)_cvtmask8_u32(_mm512_cmplt_epu64_mask(a, b));
}
#endif

#elif defined(__aarch64__)
#include <stdint.h>
#include <arm_neon.h>
#include "mask.h"

uint8x16_t hs_simdy_int8x16_eq(int8x16_t a, int8x16_t b)
{
    return vceqq_s8(a, b);
}

uint16x8_t hs_simdy_int16x8_eq(int16x8_t a, int16x8_t b)
{
    return vceqq_s16(a, b);
}

uint32x4_t hs_simdy_int32x4_eq(int32x4_t a, int32x4_t b)
{
    return vceqq_s32(a, b);
}

uint64x2_t hs_simdy_int64x2_eq(int64x2_t a, int64x2_t b)
{
    return vceqq_s64(a, b);
}

uint16_t hs_simdy_int8x16_eq_densemask(int8x16_t a, int8x16_t b)
{
    return hs_simdy_pack_mask8x16(hs_simdy_int8x16_eq(a, b));
}

uint8_t hs_simdy_int16x8_eq_densemask(int16x8_t a, int16x8_t b)
{
    return hs_simdy_pack_mask16x8(hs_simdy_int16x8_eq(a, b));
}

uint8_t hs_simdy_int32x4_eq_densemask(int32x4_t a, int32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_int32x4_eq(a, b));
}

uint8_t hs_simdy_int64x2_eq_densemask(int64x2_t a, int64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_int64x2_eq(a, b));
}

uint8x16_t hs_simdy_int8x16_lt(int8x16_t a, int8x16_t b)
{
    return vcltq_s8(a, b);
}

uint16x8_t hs_simdy_int16x8_lt(int16x8_t a, int16x8_t b)
{
    return vcltq_s16(a, b);
}

uint32x4_t hs_simdy_int32x4_lt(int32x4_t a, int32x4_t b)
{
    return vcltq_s32(a, b);
}

uint64x2_t hs_simdy_int64x2_lt(int64x2_t a, int64x2_t b)
{
    return vcltq_s64(a, b);
}

uint16_t hs_simdy_int8x16_lt_densemask(int8x16_t a, int8x16_t b)
{
    return hs_simdy_pack_mask8x16(hs_simdy_int8x16_lt(a, b));
}

uint8_t hs_simdy_int16x8_lt_densemask(int16x8_t a, int16x8_t b)
{
    return hs_simdy_pack_mask16x8(hs_simdy_int16x8_lt(a, b));
}

uint8_t hs_simdy_int32x4_lt_densemask(int32x4_t a, int32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_int32x4_lt(a, b));
}

uint8_t hs_simdy_int64x2_lt_densemask(int64x2_t a, int64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_int64x2_lt(a, b));
}

uint8x16_t hs_simdy_word8x16_lt(uint8x16_t a, uint8x16_t b)
{
    return vcltq_u8(a, b);
}

uint16x8_t hs_simdy_word16x8_lt(uint16x8_t a, uint16x8_t b)
{
    return vcltq_u16(a, b);
}

uint32x4_t hs_simdy_word32x4_lt(uint32x4_t a, uint32x4_t b)
{
    return vcltq_u32(a, b);
}

uint64x2_t hs_simdy_word64x2_lt(uint64x2_t a, uint64x2_t b)
{
    return vcltq_u64(a, b);
}

uint16_t hs_simdy_word8x16_lt_densemask(uint8x16_t a, uint8x16_t b)
{
    return hs_simdy_pack_mask8x16(hs_simdy_word8x16_lt(a, b));
}

uint8_t hs_simdy_word16x8_lt_densemask(uint16x8_t a, uint16x8_t b)
{
    return hs_simdy_pack_mask16x8(hs_simdy_word16x8_lt(a, b));
}

uint8_t hs_simdy_word32x4_lt_densemask(uint32x4_t a, uint32x4_t b)
{
    return hs_simdy_pack_mask32x4(hs_simdy_word32x4_lt(a, b));
}

uint8_t hs_simdy_word64x2_lt_densemask(uint64x2_t a, uint64x2_t b)
{
    return hs_simdy_pack_mask64x2(hs_simdy_word64x2_lt(a, b));
}

#endif
