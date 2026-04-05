#if defined(__AVX__)
#include <immintrin.h>

__attribute__((target("fma")))
__m256 hs_simdy_fmaddFloatX8(__m256 x, __m256 y, __m256 z)
{
    return _mm256_fmadd_ps(x, y, z);
}

__attribute__((target("fma")))
__m256d hs_simdy_fmaddDoubleX4(__m256d x, __m256d y, __m256d z)
{
    return _mm256_fmadd_pd(x, y, z);
}

#endif
