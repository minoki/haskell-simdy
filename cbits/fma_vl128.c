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

#elif defined(__aarch64__)
// GHC 9.6 on AArch64 is not supported

#endif
