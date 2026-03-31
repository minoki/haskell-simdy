#if defined(__SSE2__)
#include <tmmintrin.h>
#include <immintrin.h>

/*
 * negate (x :: IntN) = 1 + complement x
 *                    = (x ^ (-1)) - (-1)
 * abs(x: IntN) = if x < 0 then -x else x
 *              = let mask = x >> (N-1) -- if x < 0 then -1 else 0
 *                in (x ^ mask) - mask
 */

__m128i hs_simdy_absInt8X16(__m128i x)
{
#if defined(__SSSE3__)
    return _mm_abs_epi8(x);
#else
    __m128i zero = _mm_setzero_si128();
    __m128i mask_lo = _mm_unpacklo_epi8(zero, x); // {0, x[0], 0, x[1], ...}
    __m128i mask_hi = _mm_unpackhi_epi8(zero, x); // {0, x[8], 0, x[9], ...}
    mask_lo = _mm_srai_epi16(mask_lo, 15);
    mask_hi = _mm_srai_epi16(mask_hi, 15);
    __m128i mask = _mm_packs_epi16(mask_lo, mask_hi); // {mask_lo[0], ..., mask_lo[7], mask_hi[0], ..., mask_hi[7]}
    return _mm_sub_epi8(_mm_xor_si128(x, mask), mask);
#endif
}

__m128i hs_simdy_absInt16X8(__m128i x)
{
#if defined(__SSSE3__)
    return _mm_abs_epi16(x);
#else
    __m128i mask = _mm_srai_epi16(x, 15);
    return _mm_sub_epi16(_mm_xor_si128(x, mask), mask);
#endif
}

__m128i hs_simdy_absInt32X4(__m128i x)
{
#if defined(__SSSE3__)
    return _mm_abs_epi32(x);
#else
    __m128i mask = _mm_srai_epi32(x, 31);
    return _mm_sub_epi32(_mm_xor_si128(x, mask), mask);
#endif
}

__m128i hs_simdy_absInt64X2(__m128i x)
{
#if defined(__AVX512F__) && defined(__AVX512VL__)
    return _mm_abs_epi64(x);
#else
    __m128i mask = _mm_srai_epi32(x, 31); // {x[0].lo >> 31, x[0].hi >> 31, x[1].lo >> 31, x[1].hi >> 31}
    mask = _mm_shuffle_epi32(mask, 0xF5); // {mask[1], mask[1], mask[3], mask[3]}
    return _mm_sub_epi64(_mm_xor_si128(x, mask), mask);
#endif
}

#elif defined(__aarch64__)
#include <arm_neon.h>

int8x16_t hs_simdy_absInt8X16(int8x16_t x)
{
    return vabsq_s8(x);
}

int16x8_t hs_simdy_absInt16X8(int16x8_t x)
{
    return vabsq_s16(x);
}

int32x4_t hs_simdy_absInt32X4(int32x4_t x)
{
    return vabsq_s32(x);
}

int64x2_t hs_simdy_absInt64X2(int64x2_t x)
{
    return vabsq_s64(x);
}

#endif
