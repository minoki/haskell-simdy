#if defined(__SSE2__)
#include "mask.h"
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>

__m128i hs_simdy_select_int128(__m128i mask, __m128i then, __m128i else_)
{
#if defined(__SSE4_1__)
    return _mm_blendv_epi8(else_, then, mask);
#else
    then = _mm_and_si128(mask, then);
    else_ = _mm_andnot_si128(mask, else_);
    return _mm_or_si128(then, else_);
#endif
}

__m128i hs_simdy_select_int8x16_densemask(uint16_t mask, __m128i then, __m128i else_)
{
#if defined(__AVX512VL__)
    __mmask16 mmask = _cvtu32_mask16(mask);
    return _mm_mask_mov_epi8(else_, mmask, then);
#else
    return hs_simdy_select_int128(hs_simdy_unpack_mask8x16(mask), then, else_);
#endif
}

__m128i hs_simdy_select_int16x8_densemask(uint8_t mask, __m128i then, __m128i else_)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_epi16(else_, mmask, then);
#else
    return hs_simdy_select_int128(hs_simdy_unpack_mask16x8(mask), then, else_);
#endif
}

__m128i hs_simdy_select_int32x4_densemask(uint8_t mask, __m128i then, __m128i else_)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_epi32(else_, mmask, then);
#else
    return hs_simdy_select_int128(hs_simdy_unpack_mask32x4(mask), then, else_);
#endif
}

__m128i hs_simdy_select_int64x2_densemask(uint8_t mask, __m128i then, __m128i else_)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_epi64(else_, mmask, then);
#else
    return hs_simdy_select_int128(hs_simdy_unpack_mask64x2(mask), then, else_);
#endif
}

__m128 hs_simdy_select_floatx4(__m128 mask, __m128 then, __m128 else_)
{
#if defined(__SSE4_1__)
    return _mm_blendv_ps(else_, then, mask);
#else
    then = _mm_and_ps(mask, then);
    else_ = _mm_andnot_ps(mask, else_);
    return _mm_or_ps(then, else_);
#endif
}

__m128 hs_simdy_select_floatx4_densemask(uint8_t mask, __m128 then, __m128 else_)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_ps(else_, mmask, then);
#else
    return hs_simdy_select_floatx4(_mm_castsi128_ps(hs_simdy_unpack_mask32x4(mask)), then, else_);
#endif
}

__m128d hs_simdy_select_doublex2(__m128d mask, __m128d then, __m128d else_)
{
#if defined(__SSE4_1__)
    return _mm_blendv_pd(else_, then, mask);
#else
    then = _mm_and_pd(mask, then);
    else_ = _mm_andnot_pd(mask, else_);
    return _mm_or_pd(then, else_);
#endif
}

__m128d hs_simdy_select_doublex2_densemask(uint8_t mask, __m128d then, __m128d else_)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm_mask_mov_pd(else_, mmask, then);
#else
    return hs_simdy_select_doublex2(_mm_castsi128_pd(hs_simdy_unpack_mask64x2(mask)), then, else_);
#endif
}

#if defined(__AVX__)
__m256 hs_simdy_select_floatx8(__m256 mask, __m256 then, __m256 else_)
{
    return _mm256_blendv_ps(else_, then, mask);
}

__m256 hs_simdy_select_floatx8_densemask(uint8_t mask, __m256 then, __m256 else_)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm256_mask_mov_ps(else_, mmask, then);
#else
    return hs_simdy_select_floatx8(_mm256_castsi256_ps(hs_simdy_unpack_mask32x8(mask)), then, else_);
#endif
}

__m256d hs_simdy_select_doublex4(__m256d mask, __m256d then, __m256d else_)
{
    return _mm256_blendv_pd(else_, then, mask);
}

__m256d hs_simdy_select_doublex4_densemask(uint8_t mask, __m256d then, __m256d else_)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm256_mask_mov_pd(else_, mmask, then);
#else
    return hs_simdy_select_doublex4(_mm256_castsi256_pd(hs_simdy_unpack_mask64x4(mask)), then, else_);
#endif
}
#endif

#if defined(__AVX2__)
__m256i hs_simdy_select_int256(__m256i mask, __m256i then, __m256i else_)
{
    return _mm256_blendv_epi8(else_, then, mask);
}

__m256i hs_simdy_select_int8x32_densemask(uint32_t mask, __m256i then, __m256i else_)
{
#if defined(__AVX512VL__) && defined(__AVX512BW__)
    __mmask16 mmask = _cvtu32_mask32(mask);
    return _mm256_mask_mov_epi8(else_, mmask, then);
#else
    return hs_simdy_select_int256(hs_simdy_unpack_mask8x32(mask), then, else_);
#endif
}

__m256i hs_simdy_select_int16x16_densemask(uint16_t mask, __m256i then, __m256i else_)
{
#if defined(__AVX512VL__)
    __mmask8 mmask = _cvtu32_mask16(mask);
    return _mm256_mask_mov_epi16(else_, mmask, then);
#else
    return hs_simdy_select_int256(hs_simdy_unpack_mask16x16(mask), then, else_);
#endif
}

__m256i hs_simdy_select_int32x8_densemask(uint8_t mask, __m256i then, __m256i else_)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm256_mask_mov_epi32(else_, mmask, then);
#else
    return hs_simdy_select_int256(hs_simdy_unpack_mask32x8(mask), then, else_);
#endif
}

__m256i hs_simdy_select_int64x4_densemask(uint8_t mask, __m256i then, __m256i else_)
{
#if defined(__AVX512VL__) && defined(__AVX512DQ__)
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm256_mask_mov_epi64(else_, mmask, then);
#else
    return hs_simdy_select_int256(hs_simdy_unpack_mask64x4(mask), then, else_);
#endif
}

#endif

#if defined(__AVX512F__) /* && defined(__EVEX512__) */
__m512 hs_simdy_select_floatx16_densemask(uint16_t mask, __m512 then, __m512 else_)
{
    __mmask16 mmask = _cvtu32_mask16(mask);
    return _mm512_mask_mov_ps(else_, mmask, then);
}

#if defined(__AVX512DQ__)
__m512d hs_simdy_select_doublex8_densemask(uint8_t mask, __m512d then, __m512d else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask); // AVX512DQ
    return _mm512_mask_mov_pd(else_, mmask, then);
}
#endif

#if defined(__AVX512BW__)
__m512i hs_simdy_select_int8x64_densemask(uint64_t mask, __m512i then, __m512i else_)
{
    __mmask64 mmask = _cvtu64_mask64(mask);
    return _mm512_mask_mov_epi8(else_, mmask, then);
}

__m512i hs_simdy_select_int16x32_densemask(uint32_t mask, __m512i then, __m512i else_)
{
    __mmask32 mmask = _cvtu32_mask32(mask);
    return _mm512_mask_mov_epi16(else_, mmask, then);
}
#endif

__m512i hs_simdy_select_int32x16_densemask(uint16_t mask, __m512i then, __m512i else_)
{
    __mmask16 mmask = _cvtu32_mask16(mask);
    return _mm512_mask_mov_epi32(else_, mmask, then);
}

#if defined(__AVX512DQ__)
__m512i hs_simdy_select_int64x8_densemask(uint8_t mask, __m512i then, __m512i else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm512_mask_mov_epi64(else_, mmask, then);
}
#endif // defined(__AVX512DQ__)
#endif // defined(__AVX512F__)

#elif defined(__aarch64__)
#include <stdint.h>
#include <arm_neon.h>
#include "mask.h"

int8x16_t hs_simdy_select_int8x16(uint8x16_t mask, int8x16_t then, int8x16_t else_)
{
    return vbslq_s8(mask, then, else_);
}

int16x8_t hs_simdy_select_int16x8(uint16x8_t mask, int16x8_t then, int16x8_t else_)
{
    return vbslq_s16(mask, then, else_);
}

int32x4_t hs_simdy_select_int32x4(uint32x4_t mask, int32x4_t then, int32x4_t else_)
{
    return vbslq_s32(mask, then, else_);
}

int64x2_t hs_simdy_select_int64x2(uint64x2_t mask, int64x2_t then, int64x2_t else_)
{
    return vbslq_s64(mask, then, else_);
}

int8x16_t hs_simdy_select_int8x16_densemask(uint16_t mask, int8x16_t then, int8x16_t else_)
{
    return hs_simdy_select_int8x16(hs_simdy_unpack_mask8x16(mask), then, else_);
}

int16x8_t hs_simdy_select_int16x8_densemask(uint8_t mask, int16x8_t then, int16x8_t else_)
{
    return hs_simdy_select_int16x8(hs_simdy_unpack_mask16x8(mask), then, else_);
}

int32x4_t hs_simdy_select_int32x4_densemask(uint8_t mask, int32x4_t then, int32x4_t else_)
{
    return hs_simdy_select_int32x4(hs_simdy_unpack_mask32x4(mask), then, else_);
}

int64x2_t hs_simdy_select_int64x2_densemask(uint8_t mask, int64x2_t then, int64x2_t else_)
{
    return hs_simdy_select_int64x2(hs_simdy_unpack_mask64x2(mask), then, else_);
}

float32x4_t hs_simdy_select_floatx4(uint32x4_t mask, float32x4_t then, float32x4_t else_)
{
    return vbslq_f32(mask, then, else_);
}

float32x4_t hs_simdy_select_floatx4_densemask(uint8_t mask, float32x4_t then, float32x4_t else_)
{
    return hs_simdy_select_floatx4(hs_simdy_unpack_mask32x4(mask), then, else_);
}

float64x2_t hs_simdy_select_doublex2(uint64x2_t mask, float64x2_t then, float64x2_t else_)
{
    return vbslq_f64(mask, then, else_);
}

float64x2_t hs_simdy_select_doublex2_densemask(uint8_t mask, float64x2_t then, float64x2_t else_)
{
    return hs_simdy_select_doublex2(hs_simdy_unpack_mask64x2(mask), then, else_);
}

#endif
