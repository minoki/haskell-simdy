#if defined(__AVX2__)
#include <stdint.h>
#include <string.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "HsFFI.h"

/*
#if defined(__AVX2__)

__m256i hs_simdy_complement_int256(__m256i x)
{
    __m256i y = _mm256_set1_epi32(-1);
    return _mm256_xor_si256(x, y);
}

__m256i hs_simdy_and_int256(__m256i x, __m256i y)
{
    return _mm256_and_si256(x, y);
}

__m256i hs_simdy_or_int256(__m256i x, __m256i y)
{
    return _mm256_or_si256(x, y);
}

__m256i hs_simdy_xor_int256(__m256i x, __m256i y)
{
    return _mm256_xor_si256(x, y);
}

#endif
*/

//
// shiftL
//

__m256i hs_simdy_shiftLInt8X32(__m256i x, HsInt y)
{
    __m256i zero = _mm256_setzero_si256();
    __m256i x_lo = _mm256_unpacklo_epi8(x, zero);
    __m256i x_hi = _mm256_unpackhi_epi8(x, zero);
    __m128i count = _mm_set_epi64x(0, y);
    __m256i mask = _mm256_set1_epi16(0xff);
    x_lo = _mm256_sll_epi16(x_lo, count);
    x_lo = _mm256_and_si256(x_lo, mask);
    x_hi = _mm256_sll_epi16(x_hi, count);
    x_hi = _mm256_and_si256(x_hi, mask);
    return _mm256_packus_epi16(x_lo, x_hi);
}

__m256i hs_simdy_shiftLInt16X16(__m256i x, HsInt y)
{
    return _mm256_sll_epi16(x, _mm_set_epi64x(0, y));
}

__m256i hs_simdy_shiftLInt32X8(__m256i x, HsInt y)
{
    return _mm256_sll_epi32(x, _mm_set_epi64x(0, y));
}

__m256i hs_simdy_shiftLInt64X4(__m256i x, HsInt y)
{
    return _mm256_sll_epi64(x, _mm_set_epi64x(0, y));
}

__m256i hs_simdy_shiftLWord8X32(__m256i x, HsInt y)
{
    return hs_simdy_shiftLInt8X32(x, y);
}

__m256i hs_simdy_shiftLWord16X16(__m256i x, HsInt y)
{
    return _mm256_sll_epi16(x, _mm_set_epi64x(0, y));
}

__m256i hs_simdy_shiftLWord32X8(__m256i x, HsInt y)
{
    return _mm256_sll_epi32(x, _mm_set_epi64x(0, y));
}

__m256i hs_simdy_shiftLWord64X4(__m256i x, HsInt y)
{
    return _mm256_sll_epi64(x, _mm_set_epi64x(0, y));
}

//
// shiftR
//

__m256i hs_simdy_shiftRInt8X32(__m256i x, HsInt y)
{
    __m256i zero = _mm256_setzero_si256();
    __m256i x_lo = _mm256_unpacklo_epi8(zero, x);
    __m256i x_hi = _mm256_unpackhi_epi8(zero, x);
    __m128i count = _mm_set_epi64x(0, y + 8);
    __m256i mask = _mm256_set1_epi16(0xff);
    x_lo = _mm256_sra_epi16(x_lo, count);
    x_lo = _mm256_and_si256(x_lo, mask);
    x_hi = _mm256_sra_epi16(x_hi, count);
    x_hi = _mm256_and_si256(x_hi, mask);
    return _mm256_packus_epi16(x_lo, x_hi);
}

__m256i hs_simdy_shiftRInt16X16(__m256i x, HsInt y)
{
    return _mm256_sra_epi16(x, _mm_set_epi64x(0, y));
}

__m256i hs_simdy_shiftRInt32X8(__m256i x, HsInt y)
{
    return _mm256_sra_epi32(x, _mm_set_epi64x(0, y));
}
__m256i hs_simdy_shiftRInt64X4(__m256i x, HsInt y)
{
    _Alignas(16) int64_t buf[4];
    memcpy(buf, &x, 32);
    if (y >= 64) {
        y = 63;
    }
    buf[0] >>= y;
    buf[1] >>= y;
    buf[2] >>= y;
    buf[3] >>= y;
    memcpy(&x, buf, 32);
    return x;
}

__attribute__((target("avx512f,avx512vl")))
__m256i hs_simdy_shiftRInt64X4_avx512(__m256i x, HsInt y)
{
    return _mm256_sra_epi64(x, _mm_set_epi64x(0, y));
}

__m256i hs_simdy_shiftRWord8X32(__m256i x, HsInt y)
{
    __m256i zero = _mm256_setzero_si256();
    __m256i x_lo = _mm256_unpacklo_epi8(x, zero);
    __m256i x_hi = _mm256_unpackhi_epi8(x, zero);
    __m128i count = _mm_set_epi64x(0, y);
    __m256i mask = _mm256_set1_epi16(0xff);
    x_lo = _mm256_srl_epi16(x_lo, count);
    x_lo = _mm256_and_si256(x_lo, mask);
    x_hi = _mm256_srl_epi16(x_hi, count);
    x_hi = _mm256_and_si256(x_hi, mask);
    return _mm256_packus_epi16(x_lo, x_hi);
}

__m256i hs_simdy_shiftRWord16X16(__m256i x, HsInt y)
{
    return _mm256_srl_epi16(x, _mm_set_epi64x(0, y));
}

__m256i hs_simdy_shiftRWord32X8(__m256i x, HsInt y)
{
    return _mm256_srl_epi32(x, _mm_set_epi64x(0, y));
}

__m256i hs_simdy_shiftRWord64X4(__m256i x, HsInt y)
{
    return _mm256_srl_epi64(x, _mm_set_epi64x(0, y));
}

#endif
