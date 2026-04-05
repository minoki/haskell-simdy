#include <math.h>
#include <stdint.h>
#include <fenv.h>
#include <stdbool.h>
#if defined(__SSE2__)
#include <xmmintrin.h>
#include <immintrin.h>
#elif defined(__aarch64__)
#include <arm_neon.h>
#endif

// IEEE 754-2019 minimum/maximum:
//   * -0 < +0
//   * propagate NaN
//   * raise INVALID on signaling NaN

#pragma STDC FENV_ACCESS ON

float hs_simdy_minimumFloat(float x, float y)
{
#if defined(__aarch64__)
    float result;
    asm("fmin %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return x;
    } else if (isgreater(x, y)) {
        return y;
    } else if (isunordered(x, y)) {
        return x + y; // propagate NaN
    } else {
        // equal or both zero
        return signbit(x) ? x : y;
    }
#endif
}

float hs_simdy_maximumFloat(float x, float y)
{
#if defined(__aarch64__)
    float result;
    asm("fmax %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return y;
    } else if (isgreater(x, y)) {
        return x;
    } else if (isunordered(x, y)) {
        return x + y; // propagate NaN
    } else {
        // equal or both zero
        return signbit(x) ? y : x;
    }
#endif
}
