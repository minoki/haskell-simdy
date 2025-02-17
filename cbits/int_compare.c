#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <smmintrin.h>

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

#elif defined(__aarch64__)

#endif
