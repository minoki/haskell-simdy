#if defined(__AVX512F__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "mask.h"

uint16_t hs_simdy_eqFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_EQ_OQ));
}

uint16_t hs_simdy_ltFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_LT_OQ));
}

uint16_t hs_simdy_leFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_LE_OQ));
}

uint16_t hs_simdy_gtFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_GT_OQ));
}

uint16_t hs_simdy_geFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_GE_OQ));
}

uint16_t hs_simdy_unordFloatX16_densemask(__m512 a, __m512 b)
{
    return (uint16_t)_cvtmask16_u32(_mm512_cmp_ps_mask(a, b, _CMP_UNORD_Q));
}

#endif
