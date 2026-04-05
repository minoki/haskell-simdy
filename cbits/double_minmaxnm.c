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
double canonicalize_double(double x)
{
    asm volatile("mulsd %1, %0" : "+x"(x) : "x"(1.0));
    return x;
}
#elif defined(__aarch64__)
__attribute__((always_inline)) static inline
double canonicalize_double(double x)
{
    double result;
    asm volatile("fmul %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(1.0));
    return result;
}
#else
__attribute__((always_inline)) static inline
double canonicalize_double(double x)
{
    volatile double one = 1.0;
    return x * one;
}
#endif

double hs_simdy_minimumNumberDouble(double x, double y)
{
#if defined(__aarch64__)
    x = canonicalize_double(x);
    y = canonicalize_double(y);
    float64x1_t xx = vld1_f64(&x);
    float64x1_t yy = vld1_f64(&y);
    float64x1_t zz = vminnm_f64(xx, yy);
    double result;
    vst1_f64(&result, zz);
    return result;
    /*
    double result;
    asm("fminnm %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
    return result;
    */
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return x;
    } else if (isgreater(x, y)) {
        return y;
    } else if (isunordered(x, y)) {
        x = canonicalize_double(x);
        y = canonicalize_double(y);
        return isnan(x) ? y : x; // discard NaN
    } else {
        // equal or both zero
        return signbit(x) ? x : y;
    }
#endif
}

double hs_simdy_maximumNumberDouble(double x, double y)
{
#if defined(__aarch64__)
    x = canonicalize_double(x);
    y = canonicalize_double(y);
    float64x1_t xx = vld1_f64(&x);
    float64x1_t yy = vld1_f64(&y);
    float64x1_t zz = vmaxnm_f64(xx, yy);
    double result;
    vst1_f64(&result, zz);
    return result;
    /*
    double result;
    asm("fmaxnm %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
    return result;
    */
#else
    // portable version
    #pragma STDC FENV_ACCESS ON
    if (isless(x, y)) {
        return y;
    } else if (isgreater(x, y)) {
        return x;
    } else if (isunordered(x, y)) {
        x = canonicalize_double(x);
        y = canonicalize_double(y);
        return isnan(x) ? y : x; // discard NaN
    } else {
        // equal or both zero
        return signbit(x) ? y : x;
    }
#endif
}
