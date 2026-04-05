#if defined(__SSE2__)
#include <stdint.h>
#include <emmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>

__m128i hs_simdy_minInt8X16(__m128i a, __m128i b)
{
    __m128i mask = _mm_cmpgt_epi8(a, b);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_minInt8X16_sse41(__m128i a, __m128i b)
{
    return _mm_min_epi8(a, b);
}

__m128i hs_simdy_minInt16X8(__m128i a, __m128i b)
{
    return _mm_min_epi16(a, b);
}

__m128i hs_simdy_minInt32X4(__m128i a, __m128i b)
{
    __m128i mask = _mm_cmpgt_epi32(a, b);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_minInt32X4_sse41(__m128i a, __m128i b)
{
    return _mm_min_epi32(a, b);
}

__m128i hs_simdy_minInt64X2(__m128i a, __m128i b)
{
    int64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] < bb[1] ? aa[1] : bb[1], aa[0] < b[0] ? aa[0] : bb[0]);
}

__attribute__((target("sse4.2")))
__m128i hs_simdy_minInt64X2_sse42(__m128i a, __m128i b)
{
    __m128i mask = _mm_cmpgt_epi64(a, b);
    return _mm_blendv_epi8(a, b, mask);
}

__attribute__((target("avx512vl")))
__m128i hs_simdy_minInt64X2_avx512(__m128i a, __m128i b)
{
    return _mm_min_epi64(a, b);
}

__m128i hs_simdy_maxInt8X16(__m128i a, __m128i b)
{
    __m128i mask = _mm_cmpgt_epi8(b, a);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_maxInt8X16_sse41(__m128i a, __m128i b)
{
    return _mm_max_epi8(a, b);
}

__m128i hs_simdy_maxInt16X8(__m128i a, __m128i b)
{
    return _mm_max_epi16(a, b);
}

__m128i hs_simdy_maxInt32X4(__m128i a, __m128i b)
{
    __m128i mask = _mm_cmpgt_epi32(b, a);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_maxInt32X4_sse41(__m128i a, __m128i b)
{
    return _mm_max_epi32(a, b);
}

__m128i hs_simdy_maxInt64X2(__m128i a, __m128i b)
{
    int64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] > bb[1] ? aa[1] : bb[1], aa[0] > b[0] ? aa[0] : bb[0]);
}

__attribute__((target("sse4.2")))
__m128i hs_simdy_maxInt64X2_sse42(__m128i a, __m128i b)
{
    __m128i mask = _mm_cmpgt_epi64(b, a);
    return _mm_blendv_epi8(a, b, mask);
}

__attribute__((target("avx512vl")))
__m128i hs_simdy_maxInt64X2_avx512(__m128i a, __m128i b)
{
    return _mm_max_epi64(a, b);
}

__m128i hs_simdy_minWord8X16(__m128i a, __m128i b)
{
    return _mm_min_epu8(a, b);
}

__m128i hs_simdy_minWord16X8(__m128i a, __m128i b)
{
    __m128i signbit = _mm_set1_epi16(INT16_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi16(ax, bx);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_minWord16X8_sse41(__m128i a, __m128i b)
{
    return _mm_min_epu16(a, b);
}

__m128i hs_simdy_minWord32X4(__m128i a, __m128i b)
{
    __m128i signbit = _mm_set1_epi32(INT32_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi32(ax, bx);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_minWord32X4_sse41(__m128i a, __m128i b)
{
    return _mm_min_epu32(a, b);
}

__m128i hs_simdy_minWord64X2(__m128i a, __m128i b)
{
    uint64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] < bb[1] ? aa[1] : bb[1], aa[0] < b[0] ? aa[0] : bb[0]);
}

__attribute__((target("sse4.2")))
__m128i hs_simdy_minWord64X2_sse42(__m128i a, __m128i b)
{
    __m128i signbit = _mm_set1_epi64x(INT64_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi64(ax, bx);
    return _mm_blendv_epi8(a, b, mask);
}

__attribute__((target("avx512vl")))
__m128i hs_simdy_minWord64X2_avx512(__m128i a, __m128i b)
{
    return _mm_min_epu64(a, b);
}

__m128i hs_simdy_maxWord8X16(__m128i a, __m128i b)
{
    return _mm_max_epu8(a, b);
}

__m128i hs_simdy_maxWord16X8(__m128i a, __m128i b)
{
    __m128i signbit = _mm_set1_epi16(INT16_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi16(bx, ax);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_maxWord16X8_sse41(__m128i a, __m128i b)
{
    return _mm_max_epu16(a, b);
}

__m128i hs_simdy_maxWord32X4(__m128i a, __m128i b)
{
    __m128i signbit = _mm_set1_epi32(INT32_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi32(bx, ax);
    __m128i aa = _mm_andnot_si128(mask, a);
    __m128i bb = _mm_and_si128(mask, b);
    return _mm_or_si128(aa, bb);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_maxWord32X4_sse41(__m128i a, __m128i b)
{
    return _mm_max_epu32(a, b);
}

__m128i hs_simdy_maxWord64X2(__m128i a, __m128i b)
{
    uint64_t aa[2], bb[2];
    _mm_storeu_si128((__m128i *)aa, a);
    _mm_storeu_si128((__m128i *)bb, b);
    return _mm_set_epi64x(aa[1] > bb[1] ? aa[1] : bb[1], aa[0] > b[0] ? aa[0] : bb[0]);
}

__attribute__((target("sse4.2")))
__m128i hs_simdy_maxWord64X2_sse42(__m128i a, __m128i b)
{
    __m128i signbit = _mm_set1_epi64x(INT64_MIN);
    __m128i ax = _mm_xor_si128(a, signbit);
    __m128i bx = _mm_xor_si128(b, signbit);
    __m128i mask = _mm_cmpgt_epi64(bx, ax);
    return _mm_blendv_epi8(a, b, mask);
}

__attribute__((target("avx512vl")))
__m128i hs_simdy_maxWord64X2_avx512(__m128i a, __m128i b)
{
    return _mm_max_epu64(a, b);
}

#elif defined(__aarch64__)

// We don't support AArch64 with GHC < 9.12.
// GHC 9.12+ has min/max primitives, so we don't need to implement them in C.

#endif
