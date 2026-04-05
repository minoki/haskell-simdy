#if defined(__AVX512F__) && defined(__AVX512BW__)
#include <stdint.h>
#include <immintrin.h>

__m512i hs_simdy_minInt8X64(__m512i a, __m512i b)
{
    return _mm512_min_epi8(a, b);
}

__m512i hs_simdy_minInt16X32(__m512i a, __m512i b)
{
    return _mm512_min_epi16(a, b);
}

__m512i hs_simdy_minInt32X16(__m512i a, __m512i b)
{
    return _mm512_min_epi32(a, b);
}

__m512i hs_simdy_minInt64X8(__m512i a, __m512i b)
{
    return _mm512_min_epi64(a, b);
}

__m512i hs_simdy_maxInt8X64(__m512i a, __m512i b)
{
    return _mm512_max_epi8(a, b);
}

__m512i hs_simdy_maxInt16X32(__m512i a, __m512i b)
{
    return _mm512_max_epi16(a, b);
}

__m512i hs_simdy_maxInt32X16(__m512i a, __m512i b)
{
    return _mm512_max_epi32(a, b);
}

__m512i hs_simdy_maxInt64X8(__m512i a, __m512i b)
{
    return _mm512_max_epi64(a, b);
}

__m512i hs_simdy_minWord8X64(__m512i a, __m512i b)
{
    return _mm512_min_epu8(a, b);
}

__m512i hs_simdy_minWord16X32(__m512i a, __m512i b)
{
    return _mm512_min_epu16(a, b);
}

__m512i hs_simdy_minWord32X16(__m512i a, __m512i b)
{
    return _mm512_min_epu32(a, b);
}

__m512i hs_simdy_minWord64X8(__m512i a, __m512i b)
{
    return _mm512_min_epu64(a, b);
}

__m512i hs_simdy_maxWord8X64(__m512i a, __m512i b)
{
    return _mm512_max_epu8(a, b);
}

__m512i hs_simdy_maxWord16X32(__m512i a, __m512i b)
{
    return _mm512_max_epu16(a, b);
}

__m512i hs_simdy_maxWord32X16(__m512i a, __m512i b)
{
    return _mm512_max_epu32(a, b);
}

__m512i hs_simdy_maxWord64X8(__m512i a, __m512i b)
{
    return _mm512_max_epu64(a, b);
}

#endif
