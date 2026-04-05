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

// IEEE 754-2019 minimumNumber/maximumNumber:
//   * -0 < +0
//   * NaN is missing data
//   * raise INVALID on signaling NaN

#pragma STDC FENV_ACCESS ON

// Convert (possible) signaling NaN to quiet one
#if defined(__SSE2__)
__attribute__((always_inline)) static inline
float canonicalize_float(float x)
{
    asm volatile("mulss %1, %0" : "+x"(x) : "x"(1.0f));
    return x;
}
#elif defined(__aarch64__)
__attribute__((always_inline)) static inline
float canonicalize_float(float x)
{
    float result;
    asm volatile("fmul %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(1.0f));
    return result;
}
#else
__attribute__((always_inline)) static inline
float canonicalize_float(float x)
{
    volatile float one = 1.0f;
    return x * one;
}
#endif

float hs_simdy_minimumNumberFloat(float x, float y)
{
#if defined(__aarch64__)
    float result;
    x = canonicalize_float(x);
    y = canonicalize_float(y);
    asm("fminnm %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return x;
    } else if (isgreater(x, y)) {
        return y;
    } else if (isunordered(x, y)) {
        x = canonicalize_float(x);
        y = canonicalize_float(y);
        return isnan(x) ? y : x; // discard NaN
    } else {
        // equal or both zero
        return signbit(x) ? x : y;
    }
#endif
}

float hs_simdy_maximumNumberFloat(float x, float y)
{
#if defined(__aarch64__)
    float result;
    x = canonicalize_float(x);
    y = canonicalize_float(y);
    asm("fmaxnm %s0, %s1, %s2" : "=w"(result) : "w"(x), "w"(y));
    return result;
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return y;
    } else if (isgreater(x, y)) {
        return x;
    } else if (isunordered(x, y)) {
        x = canonicalize_float(x);
        y = canonicalize_float(y);
        return isnan(x) ? y : x; // discard NaN
    } else {
        // equal or both zero
        return signbit(x) ? y : x;
    }
#endif
}
