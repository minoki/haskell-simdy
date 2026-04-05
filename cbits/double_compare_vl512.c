#if defined(__AVX512F__) && defined(__AVX512DQ__)
#include <stdint.h>
#include <emmintrin.h>
#include <immintrin.h>
#include "mask.h"

uint8_t hs_simdy_eqDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_EQ_OQ));
}

uint8_t hs_simdy_ltDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_LT_OQ));
}

uint8_t hs_simdy_leDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_LE_OQ));
}

uint8_t hs_simdy_gtDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_GT_OQ));
}

uint8_t hs_simdy_geDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_GE_OQ));
}

uint8_t hs_simdy_unordDoubleX8_densemask(__m512d a, __m512d b)
{
    return (uint8_t)_cvtmask16_u32(_mm512_cmp_pd_mask(a, b, _CMP_UNORD_Q));
}

#endif
