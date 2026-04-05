#if defined(__AVX512F__) && defined(__AVX512BW__)
#include <stdint.h>
#include <string.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "HsFFI.h"

/*
#if defined(__AVX512F__)

__m512i hs_simdy_complement_int512(__m512i x)
{
    __m512i y = _mm512_set1_epi32(-1);
    return _mm512_xor_si512(x, y);
}

__m512i hs_simdy_and_int512(__m512i x, __m512i y)
{
    return _mm512_and_si512(x, y);
}

__m512i hs_simdy_or_int512(__m512i x, __m512i y)
{
    return _mm512_or_si512(x, y);
}

__m512i hs_simdy_xor_int512(__m512i x, __m512i y)
{
    return _mm512_xor_si512(x, y);
}

#endif
*/

//
// shiftL
//

__m512i hs_simdy_shiftLInt8X64(__m512i x, HsInt y)
{
    __m512i zero = _mm512_setzero_si512();
    __m512i x_lo = _mm512_unpacklo_epi8(x, zero);
    __m512i x_hi = _mm512_unpackhi_epi8(x, zero);
    __m128i count = _mm_set_epi64x(0, y);
    __m512i mask = _mm512_set1_epi16(0xff);
    x_lo = _mm512_sll_epi16(x_lo, count);
    x_lo = _mm512_and_si512(x_lo, mask);
    x_hi = _mm512_sll_epi16(x_hi, count);
    x_hi = _mm512_and_si512(x_hi, mask);
    return _mm512_packus_epi16(x_lo, x_hi);
}

__m512i hs_simdy_shiftLInt16X32(__m512i x, HsInt y)
{
    return _mm512_sll_epi16(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftLInt32X16(__m512i x, HsInt y)
{
    return _mm512_sll_epi32(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftLInt64X8(__m512i x, HsInt y)
{
    return _mm512_sll_epi64(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftLWord8X64(__m512i x, HsInt y)
{
    return hs_simdy_shiftLInt8X64(x, y);
}

__m512i hs_simdy_shiftLWord16X32(__m512i x, HsInt y)
{
    return _mm512_sll_epi16(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftLWord32X16(__m512i x, HsInt y)
{
    return _mm512_sll_epi32(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftLWord64X8(__m512i x, HsInt y)
{
    return _mm512_sll_epi64(x, _mm_set_epi64x(0, y));
}

//
// shiftR
//

__m512i hs_simdy_shiftRInt8X64(__m512i x, HsInt y)
{
    __m512i zero = _mm512_setzero_si512();
    __m512i x_lo = _mm512_unpacklo_epi8(zero, x);
    __m512i x_hi = _mm512_unpackhi_epi8(zero, x);
    __m128i count = _mm_set_epi64x(0, y + 8);
    __m512i mask = _mm512_set1_epi16(0xff);
    x_lo = _mm512_sra_epi16(x_lo, count);
    x_lo = _mm512_and_si512(x_lo, mask);
    x_hi = _mm512_sra_epi16(x_hi, count);
    x_hi = _mm512_and_si512(x_hi, mask);
    return _mm512_packus_epi16(x_lo, x_hi);
}

__m512i hs_simdy_shiftRInt16X32(__m512i x, HsInt y)
{
    return _mm512_sra_epi16(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftRInt32X16(__m512i x, HsInt y)
{
    return _mm512_sra_epi32(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftRInt64X8(__m512i x, HsInt y)
{
    return _mm512_sra_epi64(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftRWord8X64(__m512i x, HsInt y)
{
    __m512i zero = _mm512_setzero_si512();
    __m512i x_lo = _mm512_unpacklo_epi8(x, zero);
    __m512i x_hi = _mm512_unpackhi_epi8(x, zero);
    __m128i count = _mm_set_epi64x(0, y);
    __m512i mask = _mm512_set1_epi16(0xff);
    x_lo = _mm512_srl_epi16(x_lo, count);
    x_lo = _mm512_and_si512(x_lo, mask);
    x_hi = _mm512_srl_epi16(x_hi, count);
    x_hi = _mm512_and_si512(x_hi, mask);
    return _mm512_packus_epi16(x_lo, x_hi);
}

__m512i hs_simdy_shiftRWord16X32(__m512i x, HsInt y)
{
    return _mm512_srl_epi16(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftRWord32X16(__m512i x, HsInt y)
{
    return _mm512_srl_epi32(x, _mm_set_epi64x(0, y));
}

__m512i hs_simdy_shiftRWord64X8(__m512i x, HsInt y)
{
    return _mm512_srl_epi64(x, _mm_set_epi64x(0, y));
}

#endif
