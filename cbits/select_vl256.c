#if defined(__AVX__)
#include "mask.h"
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>

__m256 hs_simdy_selectFloatX8(__m256 mask, __m256 then, __m256 else_)
{
    return _mm256_blendv_ps(else_, then, mask);
}

__m256 hs_simdy_selectFloatX8_densemask(uint8_t mask, __m256 then, __m256 else_)
{
    return hs_simdy_selectFloatX8(_mm256_castsi256_ps(hs_simdy_unpack_mask32x8(mask)), then, else_);
}

__attribute__((target("avx512vl,avx512dq")))
__m256 hs_simdy_selectFloatX8_densemask_avx512(uint8_t mask, __m256 then, __m256 else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm256_mask_mov_ps(else_, mmask, then);
}

__m256d hs_simdy_selectDoubleX4(__m256d mask, __m256d then, __m256d else_)
{
    return _mm256_blendv_pd(else_, then, mask);
}

__m256d hs_simdy_selectDoubleX4_densemask(uint8_t mask, __m256d then, __m256d else_)
{
    return hs_simdy_selectDoubleX4(_mm256_castsi256_pd(hs_simdy_unpack_mask64x4(mask)), then, else_);
}

__attribute__((target("avx512vl,avx512dq")))
__m256d hs_simdy_selectDoubleX4_densemask_avx512(uint8_t mask, __m256d then, __m256d else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm256_mask_mov_pd(else_, mmask, then);
}

#if defined(__AVX2__)
__m256i hs_simdy_selectInt256(__m256i mask, __m256i then, __m256i else_)
{
    return _mm256_blendv_epi8(else_, then, mask);
}

__m256i hs_simdy_selectInt8X32_densemask(uint32_t mask, __m256i then, __m256i else_)
{
    return hs_simdy_selectInt256(hs_simdy_unpack_mask8x32(mask), then, else_);
}

__attribute__((target("avx512vl,avx512bw")))
__m256i hs_simdy_selectInt8X32_densemask_avx512(uint32_t mask, __m256i then, __m256i else_)
{
    __mmask32 mmask = _cvtu32_mask32(mask);
    return _mm256_mask_mov_epi8(else_, mmask, then);
}

__m256i hs_simdy_selectInt16X16_densemask(uint16_t mask, __m256i then, __m256i else_)
{
    return hs_simdy_selectInt256(hs_simdy_unpack_mask16x16(mask), then, else_);
}

__attribute__((target("avx512vl,avx512bw")))
__m256i hs_simdy_selectInt16X16_densemask_avx512(uint16_t mask, __m256i then, __m256i else_)
{
    __mmask16 mmask = _cvtu32_mask16(mask);
    return _mm256_mask_mov_epi16(else_, mmask, then);
}

__m256i hs_simdy_selectInt32X8_densemask(uint8_t mask, __m256i then, __m256i else_)
{
    return hs_simdy_selectInt256(hs_simdy_unpack_mask32x8(mask), then, else_);
}

__attribute__((target("avx512vl,avx512dq")))
__m256i hs_simdy_selectInt32X8_densemask_avx512(uint8_t mask, __m256i then, __m256i else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm256_mask_mov_epi32(else_, mmask, then);
}

__m256i hs_simdy_selectInt64X4_densemask(uint8_t mask, __m256i then, __m256i else_)
{
    return hs_simdy_selectInt256(hs_simdy_unpack_mask64x4(mask), then, else_);
}

__attribute__((target("avx512vl,avx512dq")))
__m256i hs_simdy_selectInt64X4_densemask_avx512(uint8_t mask, __m256i then, __m256i else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm256_mask_mov_epi64(else_, mmask, then);
}

#endif // defined(__AVX2__)

#endif // defined(__AVX__)
