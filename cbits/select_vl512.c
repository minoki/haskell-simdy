#if defined(__AVX512F__) /* && defined(__EVEX512__) */
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>

__m512 hs_simdy_selectFloatX16_densemask(uint16_t mask, __m512 then, __m512 else_)
{
    __mmask16 mmask = _cvtu32_mask16(mask);
    return _mm512_mask_mov_ps(else_, mmask, then);
}

#if defined(__AVX512DQ__)
__m512d hs_simdy_selectDoubleX8_densemask(uint8_t mask, __m512d then, __m512d else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask); // AVX512DQ
    return _mm512_mask_mov_pd(else_, mmask, then);
}
#endif

#if defined(__AVX512BW__)
__m512i hs_simdy_selectInt8X64_densemask(uint64_t mask, __m512i then, __m512i else_)
{
    __mmask64 mmask = _cvtu64_mask64(mask);
    return _mm512_mask_mov_epi8(else_, mmask, then);
}

__m512i hs_simdy_selectInt16X32_densemask(uint32_t mask, __m512i then, __m512i else_)
{
    __mmask32 mmask = _cvtu32_mask32(mask);
    return _mm512_mask_mov_epi16(else_, mmask, then);
}
#endif

__m512i hs_simdy_selectInt32X16_densemask(uint16_t mask, __m512i then, __m512i else_)
{
    __mmask16 mmask = _cvtu32_mask16(mask);
    return _mm512_mask_mov_epi32(else_, mmask, then);
}

#if defined(__AVX512DQ__)
__m512i hs_simdy_selectInt64X8_densemask(uint8_t mask, __m512i then, __m512i else_)
{
    __mmask8 mmask = _cvtu32_mask8(mask);
    return _mm512_mask_mov_epi64(else_, mmask, then);
}
#endif // defined(__AVX512DQ__)
#endif // defined(__AVX512F__)
