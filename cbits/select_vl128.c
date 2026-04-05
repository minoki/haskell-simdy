#if defined(__SSE2__)
#include "mask.h"
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>

__m128i hs_simdy_selectInt128(__m128i mask, __m128i then, __m128i else_)
{
    then = _mm_and_si128(mask, then);
    else_ = _mm_andnot_si128(mask, else_);
    return _mm_or_si128(then, else_);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_selectInt128_sse41(__m128i mask, __m128i then, __m128i else_)
{
    return _mm_blendv_epi8(else_, then, mask);
}

__m128i hs_simdy_selectInt8X16_densemask(uint16_t mask, __m128i then, __m128i else_)
{
    return hs_simdy_selectInt128(hs_simdy_unpack_mask8x16(mask), then, else_);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_selectInt8X16_densemask_sse41(uint16_t mask, __m128i then, __m128i else_)
{
    return hs_simdy_selectInt128_sse41(hs_simdy_unpack_mask8x16(mask), then, else_);
}

__attribute__((target("avx512vl,avx512bw")))
__m128i hs_simdy_selectInt8X16_densemask_avx512(uint16_t mask, __m128i then, __m128i else_)
{
    __mmask16 mmask = _cvtu32_mask16(mask);
    return _mm_mask_mov_epi8(else_, mmask, then);
}

__m128i hs_simdy_selectInt16X8_densemask(uint8_t mask, __m128i then, __m128i else_)
{
    return hs_simdy_selectInt128(hs_simdy_unpack_mask16x8(mask), then, else_);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_selectInt16X8_densemask_sse41(uint8_t mask, __m128i then, __m128i else_)
{
    return hs_simdy_selectInt128_sse41(hs_simdy_unpack_mask16x8(mask), then, else_);
}

__attribute__((target("avx512vl,avx512bw,avx512dq")))
__m128i hs_simdy_selectInt16X8_densemask_avx512(uint8_t mask, __m128i then, __m128i else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_epi16(else_, mmask, then);
}

__m128i hs_simdy_selectInt32X4_densemask(uint8_t mask, __m128i then, __m128i else_)
{
    return hs_simdy_selectInt128(hs_simdy_unpack_mask32x4(mask), then, else_);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_selectInt32X4_densemask_sse41(uint8_t mask, __m128i then, __m128i else_)
{
    return hs_simdy_selectInt128_sse41(hs_simdy_unpack_mask32x4(mask), then, else_);
}

__attribute__((target("avx512vl,avx512dq")))
__m128i hs_simdy_selectInt32X4_densemask_avx512(uint8_t mask, __m128i then, __m128i else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_epi32(else_, mmask, then);
}

__m128i hs_simdy_selectInt64X2_densemask(uint8_t mask, __m128i then, __m128i else_)
{
    return hs_simdy_selectInt128(hs_simdy_unpack_mask64x2(mask), then, else_);
}

__attribute__((target("sse4.1")))
__m128i hs_simdy_selectInt64X2_densemask_sse41(uint8_t mask, __m128i then, __m128i else_)
{
    return hs_simdy_selectInt128(hs_simdy_unpack_mask64x2(mask), then, else_);
}

__attribute__((target("avx512vl,avx512dq")))
__m128i hs_simdy_selectInt64X2_densemask_avx512(uint8_t mask, __m128i then, __m128i else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_epi64(else_, mmask, then);
}

__m128 hs_simdy_selectFloatX4(__m128 mask, __m128 then, __m128 else_)
{
    then = _mm_and_ps(mask, then);
    else_ = _mm_andnot_ps(mask, else_);
    return _mm_or_ps(then, else_);
}

__attribute__((target("sse4.1")))
__m128 hs_simdy_selectFloatX4_sse41(__m128 mask, __m128 then, __m128 else_)
{
    return _mm_blendv_ps(else_, then, mask);
}

__m128 hs_simdy_selectFloatX4_densemask(uint8_t mask, __m128 then, __m128 else_)
{
    return hs_simdy_selectFloatX4(_mm_castsi128_ps(hs_simdy_unpack_mask32x4(mask)), then, else_);
}

__attribute__((target("sse4.1")))
__m128 hs_simdy_selectFloatX4_densemask_sse41(uint8_t mask, __m128 then, __m128 else_)
{
    return hs_simdy_selectFloatX4_sse41(_mm_castsi128_ps(hs_simdy_unpack_mask32x4(mask)), then, else_);
}

__attribute__((target("avx512vl,avx512dq")))
__m128 hs_simdy_selectFloatX4_densemask_avx512(uint8_t mask, __m128 then, __m128 else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_ps(else_, mmask, then);
}

__m128d hs_simdy_selectDoubleX2(__m128d mask, __m128d then, __m128d else_)
{
    then = _mm_and_pd(mask, then);
    else_ = _mm_andnot_pd(mask, else_);
    return _mm_or_pd(then, else_);
}

__attribute__((target("sse4.1")))
__m128d hs_simdy_selectDoubleX2_sse41(__m128d mask, __m128d then, __m128d else_)
{
    return _mm_blendv_pd(else_, then, mask);
}

__m128d hs_simdy_selectDoubleX2_densemask(uint8_t mask, __m128d then, __m128d else_)
{
    return hs_simdy_selectDoubleX2(_mm_castsi128_pd(hs_simdy_unpack_mask64x2(mask)), then, else_);
}

__attribute__((target("sse4.1")))
__m128d hs_simdy_selectDoubleX2_densemask_sse41(uint8_t mask, __m128d then, __m128d else_)
{
    return hs_simdy_selectDoubleX2_sse41(_mm_castsi128_pd(hs_simdy_unpack_mask64x2(mask)), then, else_);
}

__attribute__((target("avx512vl,avx512dq")))
__m128d hs_simdy_selectDoubleX2_densemask_avx512(uint8_t mask, __m128d then, __m128d else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_pd(else_, mmask, then);
}

#elif defined(__aarch64__)
#include <stdint.h>
#include <arm_neon.h>
#include "mask.h"

int8x16_t hs_simdy_selectInt8X16(uint8x16_t mask, int8x16_t then, int8x16_t else_)
{
    return vbslq_s8(mask, then, else_);
}

int16x8_t hs_simdy_selectInt16X8(uint16x8_t mask, int16x8_t then, int16x8_t else_)
{
    return vbslq_s16(mask, then, else_);
}

int32x4_t hs_simdy_selectInt32X4(uint32x4_t mask, int32x4_t then, int32x4_t else_)
{
    return vbslq_s32(mask, then, else_);
}

int64x2_t hs_simdy_selectInt64X2(uint64x2_t mask, int64x2_t then, int64x2_t else_)
{
    return vbslq_s64(mask, then, else_);
}

int8x16_t hs_simdy_selectInt8X16_densemask(uint16_t mask, int8x16_t then, int8x16_t else_)
{
    return hs_simdy_selectInt8X16(hs_simdy_unpack_mask8x16(mask), then, else_);
}

int16x8_t hs_simdy_selectInt16X8_densemask(uint8_t mask, int16x8_t then, int16x8_t else_)
{
    return hs_simdy_selectInt16X8(hs_simdy_unpack_mask16x8(mask), then, else_);
}

int32x4_t hs_simdy_selectInt32X4_densemask(uint8_t mask, int32x4_t then, int32x4_t else_)
{
    return hs_simdy_selectInt32X4(hs_simdy_unpack_mask32x4(mask), then, else_);
}

int64x2_t hs_simdy_selectInt64X2_densemask(uint8_t mask, int64x2_t then, int64x2_t else_)
{
    return hs_simdy_selectInt64X2(hs_simdy_unpack_mask64x2(mask), then, else_);
}

float32x4_t hs_simdy_selectFloatX4(uint32x4_t mask, float32x4_t then, float32x4_t else_)
{
    return vbslq_f32(mask, then, else_);
}

float32x4_t hs_simdy_selectFloatX4_densemask(uint8_t mask, float32x4_t then, float32x4_t else_)
{
    return hs_simdy_selectFloatX4(hs_simdy_unpack_mask32x4(mask), then, else_);
}

float64x2_t hs_simdy_selectDoubleX2(uint64x2_t mask, float64x2_t then, float64x2_t else_)
{
    return vbslq_f64(mask, then, else_);
}

float64x2_t hs_simdy_selectDoubleX2_densemask(uint8_t mask, float64x2_t then, float64x2_t else_)
{
    return hs_simdy_selectDoubleX2(hs_simdy_unpack_mask64x2(mask), then, else_);
}

#endif
