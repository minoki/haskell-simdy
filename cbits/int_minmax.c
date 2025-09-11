#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>

__m128i hs_simdy_minInt8X16(__m128i a, __m128i b)
{
#if defined(__SSE4_1__)
    return _mm_min_epi8(a, b);
#else
    __m128i mask = _mm_cmpgt_epi8(a, b);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
#endif
}

__m128i hs_simdy_minInt16X8(__m128i a, __m128i b)
{
    return _mm_min_epi16(a, b);
}

__m128i hs_simdy_minInt32X4(__m128i a, __m128i b)
{
#if defined(__SSE4_1__)
    return _mm_min_epi32(a, b);
#else
    __m128i mask = _mm_cmpgt_epi32(a, b);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
#endif
}

__m128i hs_simdy_minInt64X2(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return _mm_min_epi64(a, b);
#elif defined(__SSE4_2__)
    __m128i mask = _mm_cmpgt_epi64(a, b);
    return _mm_blendv_epi8(a, b, mask);
#else
    int64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] < bb[1] ? aa[1] : bb[1], aa[0] < b[0] ? aa[0] : bb[0]);
#endif
}

#if defined(__AVX2__)
__m256i hs_simdy_minInt8X32(__m256i a, __m256i b)
{
    return _mm256_min_epi8(a, b);
}

__m256i hs_simdy_minInt16X16(__m256i a, __m256i b)
{
    return _mm256_min_epi16(a, b);
}

__m256i hs_simdy_minInt32X8(__m256i a, __m256i b)
{
    return _mm256_min_epi32(a, b);
}

__m256i hs_simdy_minInt64X4(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return _mm256_min_epi64(a, b);
#else // AVX2
    __m256i mask = _mm256_cmpgt_epi64(a, b);
    return _mm256_blendv_epi8(a, b, mask);
#endif
}
#endif

#if defined(__AVX512F__) && defined(__AVX512BW__)
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
#endif

__m128i hs_simdy_maxInt8X16(__m128i a, __m128i b)
{
#if defined(__SSE4_1__)
    return _mm_max_epi8(a, b);
#else
    __m128i mask = _mm_cmpgt_epi8(b, a);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
#endif
}

__m128i hs_simdy_maxInt16X8(__m128i a, __m128i b)
{
    return _mm_max_epi16(a, b);
}

__m128i hs_simdy_maxInt32X4(__m128i a, __m128i b)
{
#if defined(__SSE4_1__)
    return _mm_max_epi32(a, b);
#else
    __m128i mask = _mm_cmpgt_epi32(b, a);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
#endif
}

__m128i hs_simdy_maxInt64X2(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return _mm_max_epi64(a, b);
#elif defined(__SSE4_2__)
    __m128i mask = _mm_cmpgt_epi64(b, a);
    return _mm_blendv_epi8(a, b, mask);
#else
    int64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] > bb[1] ? aa[1] : bb[1], aa[0] > b[0] ? aa[0] : bb[0]);
#endif
}

#if defined(__AVX2__)
__m256i hs_simdy_maxInt8X32(__m256i a, __m256i b)
{
    return _mm256_max_epi8(a, b);
}

__m256i hs_simdy_maxInt16X16(__m256i a, __m256i b)
{
    return _mm256_max_epi16(a, b);
}

__m256i hs_simdy_maxInt32X8(__m256i a, __m256i b)
{
    return _mm256_max_epi32(a, b);
}

__m256i hs_simdy_maxInt64X4(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return _mm256_max_epi64(a, b);
#else // AVX2
    __m256i mask = _mm256_cmpgt_epi64(b, a);
    return _mm256_blendv_epi8(a, b, mask);
#endif
}
#endif

#if defined(__AVX512F__) && defined(__AVX512BW__)
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
#endif

__m128i hs_simdy_minWord8X16(__m128i a, __m128i b)
{
    return _mm_min_epu8(a, b);
}

__m128i hs_simdy_minWord16X8(__m128i a, __m128i b)
{
#if defined(__SSE4_1__)
    return _mm_min_epu16(a, b);
#else
    __m128i signbit = _mm_set1_epi16(INT16_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi16(ax, bx);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
#endif
}

__m128i hs_simdy_minWord32X4(__m128i a, __m128i b)
{
#if defined(__SSE4_1__)
    return _mm_min_epu32(a, b);
#else
    __m128i signbit = _mm_set1_epi32(INT32_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi32(ax, bx);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
#endif
}

__m128i hs_simdy_minWord64X2(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return _mm_min_epu64(a, b);
#elif defined(__SSE4_2__)
    __m128i signbit = _mm_set1_epi64x(INT64_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi64(ax, bx);
    return _mm_blendv_epi8(a, b, mask);
#else
    uint64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] < bb[1] ? aa[1] : bb[1], aa[0] < b[0] ? aa[0] : bb[0]);
#endif
}

#if defined(__AVX2__)
__m256i hs_simdy_minWord8X32(__m256i a, __m256i b)
{
    return _mm256_min_epu8(a, b);
}

__m256i hs_simdy_minWord16X16(__m256i a, __m256i b)
{
    return _mm256_min_epu16(a, b);
}

__m256i hs_simdy_minWord32X8(__m256i a, __m256i b)
{
    return _mm256_min_epu32(a, b);
}

__m256i hs_simdy_minWord64X4(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return _mm256_min_epu64(a, b);
#else // AVX2
    __m256i signbit = _mm256_set1_epi64x(INT64_MIN);
    __m256i ax = _mm256_xor_si256(a, signbit);
    __m256i bx = _mm256_xor_si256(b, signbit);
    __m256i mask = _mm256_cmpgt_epi64(ax, bx);
    return _mm256_blendv_epi8(a, b, mask);
#endif
}
#endif

#if defined(__AVX512F__) && defined(__AVX512BW__)
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
#endif

__m128i hs_simdy_maxWord8X16(__m128i a, __m128i b)
{
    return _mm_max_epu8(a, b);
}

__m128i hs_simdy_maxWord16X8(__m128i a, __m128i b)
{
#if defined(__SSE4_1__)
    return _mm_max_epu16(a, b);
#else
    __m128i signbit = _mm_set1_epi16(INT16_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi16(bx, ax);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
#endif
}

__m128i hs_simdy_maxWord32X4(__m128i a, __m128i b)
{
#if defined(__SSE4_1__)
    return _mm_max_epu32(a, b);
#else
    __m128i signbit = _mm_set1_epi32(INT32_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi32(bx, ax);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
#endif
}

__m128i hs_simdy_maxWord64X2(__m128i a, __m128i b)
{
#if defined(__AVX512VL__)
    return _mm_max_epu64(a, b);
#elif defined(__SSE4_2__)
    __m128i signbit = _mm_set1_epi64x(INT64_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi64(bx, ax);
    return _mm_blendv_epi8(a, b, mask);
#else
    uint64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] > bb[1] ? aa[1] : bb[1], aa[0] > b[0] ? aa[0] : bb[0]);
#endif
}

#if defined(__AVX2__)
__m256i hs_simdy_maxWord8X32(__m256i a, __m256i b)
{
    return _mm256_max_epu8(a, b);
}

__m256i hs_simdy_maxWord16X16(__m256i a, __m256i b)
{
    return _mm256_max_epu16(a, b);
}

__m256i hs_simdy_maxWord32X8(__m256i a, __m256i b)
{
    return _mm256_max_epu32(a, b);
}

__m256i hs_simdy_maxWord64X4(__m256i a, __m256i b)
{
#if defined(__AVX512VL__)
    return _mm256_max_epu64(a, b);
#else // AVX2
    __m256i signbit = _mm256_set1_epi64x(INT64_MIN);
    __m256i ax = _mm256_xor_si256(a, signbit);
    __m256i bx = _mm256_xor_si256(b, signbit);
    __m256i mask = _mm256_cmpgt_epi64(bx, ax);
    return _mm256_blendv_epi8(a, b, mask);
#endif
}
#endif

#if defined(__AVX512F__) && defined(__AVX512BW__)
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

#elif defined(__aarch64__)

// We don't support AArch64 with GHC < 9.12.
// GHC 9.12+ has min/max primitives, so we don't need to implement them in C.

#endif
