#if defined(__AVX2__)
#include <stdint.h>
#include <immintrin.h>

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
    __m256i mask = _mm256_cmpgt_epi64(a, b);
    return _mm256_blendv_epi8(a, b, mask);
}

__attribute__((target("avx512vl")))
__m256i hs_simdy_minInt64X4_avx512(__m256i a, __m256i b)
{
    return _mm256_min_epi64(a, b);
}

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
    __m256i mask = _mm256_cmpgt_epi64(b, a);
    return _mm256_blendv_epi8(a, b, mask);
}

__attribute__((target("avx512vl")))
__m256i hs_simdy_maxInt64X4_avx512(__m256i a, __m256i b)
{
    return _mm256_max_epi64(a, b);
}

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
    __m256i signbit = _mm256_set1_epi64x(INT64_MIN);
    __m256i ax = _mm256_xor_si256(a, signbit);
    __m256i bx = _mm256_xor_si256(b, signbit);
    __m256i mask = _mm256_cmpgt_epi64(ax, bx);
    return _mm256_blendv_epi8(a, b, mask);
}

__attribute__((target("avx512vl")))
__m256i hs_simdy_minWord64X4_avx512(__m256i a, __m256i b)
{
    return _mm256_min_epu64(a, b);
}

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
    __m256i signbit = _mm256_set1_epi64x(INT64_MIN);
    __m256i ax = _mm256_xor_si256(a, signbit);
    __m256i bx = _mm256_xor_si256(b, signbit);
    __m256i mask = _mm256_cmpgt_epi64(bx, ax);
    return _mm256_blendv_epi8(a, b, mask);
}

__attribute__((target("avx512vl")))
__m256i hs_simdy_maxWord64X4_avx512(__m256i a, __m256i b)
{
    return _mm256_max_epu64(a, b);
}

#endif
