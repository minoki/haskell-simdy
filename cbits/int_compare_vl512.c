#if defined(__AVX512F__) && defined(__AVX512BW__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "mask.h"

uint64_t hs_simdy_eqInt8X64_densemask(__m512i a, __m512i b)
{
    return _cvtmask64_u64(_mm512_cmpeq_epi8_mask(a, b));
}

uint32_t hs_simdy_eqInt16X32_densemask(__m512i a, __m512i b)
{
    return _cvtmask32_u32(_mm512_cmpeq_epi16_mask(a, b));
}

uint16_t hs_simdy_eqInt32X16_densemask(__m512i a, __m512i b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmpeq_epi32_mask(a, b));
}

uint8_t hs_simdy_eqInt64X8_densemask(__m512i a, __m512i b)
{
    return (uint8_t)_cvtmask8_u32(_mm512_cmpeq_epi64_mask(a, b));
}

uint64_t hs_simdy_ltInt8X64_densemask(__m512i a, __m512i b)
{
    return _cvtmask64_u64(_mm512_cmplt_epi8_mask(a, b));
}

uint32_t hs_simdy_ltInt16X32_densemask(__m512i a, __m512i b)
{
    return _cvtmask32_u32(_mm512_cmplt_epi16_mask(a, b));
}

uint16_t hs_simdy_ltInt32X16_densemask(__m512i a, __m512i b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmplt_epi32_mask(a, b));
}

uint8_t hs_simdy_ltInt64X8_densemask(__m512i a, __m512i b)
{
    return (uint8_t)_cvtmask8_u32(_mm512_cmplt_epi64_mask(a, b));
}

uint64_t hs_simdy_ltWord8X64_densemask(__m512i a, __m512i b)
{
    return _cvtmask64_u64(_mm512_cmplt_epu8_mask(a, b));
}

uint32_t hs_simdy_ltWord16X32_densemask(__m512i a, __m512i b)
{
    return _cvtmask32_u32(_mm512_cmplt_epu16_mask(a, b));
}

uint16_t hs_simdy_ltWord32X16_densemask(__m512i a, __m512i b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmplt_epu32_mask(a, b));
}

uint8_t hs_simdy_ltWord64X8_densemask(__m512i a, __m512i b)
{
    return (uint8_t)_cvtmask8_u32(_mm512_cmplt_epu64_mask(a, b));
}

#endif
