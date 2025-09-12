#include <math.h>
#include <stdint.h>
#include <fenv.h>
#include <stdbool.h>

#if defined(__SSE2__)
#include <xmmintrin.h>
#include <immintrin.h>

__attribute__((target("fma")))
__m128 hs_simdy_fmaddFloatX4(__m128 x, __m128 y, __m128 z)
{
    return _mm_fmadd_ps(x, y, z);
}

__attribute__((target("fma")))
__m128d hs_simdy_fmaddDoubleX2(__m128d x, __m128d y, __m128d z)
{
    return _mm_fmadd_pd(x, y, z);
}

#if defined(__AVX__)
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

#if defined(__AVX512F__)
__m512 hs_simdy_fmaddFloatX16(__m512 x, __m512 y, __m512 z)
{
    return _mm512_fmadd_ps(x, y, z);
}

__m512d hs_simdy_fmaddDoubleX8(__m512d x, __m512d y, __m512d z)
{
    return _mm512_fmadd_pd(x, y, z);
}
#endif

#elif defined(__aarch64__)
// GHC 9.6 on AArch64 is not supported

#endif
