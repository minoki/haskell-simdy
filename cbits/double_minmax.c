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

double hs_simdy_minimumDouble(double x, double y)
{
#if defined(__aarch64__)
    float64x1_t xx = vld1_f64(&x);
    float64x1_t yy = vld1_f64(&y);
    float64x1_t zz = vmin_f64(xx, yy);
    double result;
    vst1_f64(&result, zz);
    return result;
    /*
    double result;
    asm("fmin %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
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
        return x + y; // propagate NaN
    } else {
        // equal or both zero
        return signbit(x) ? x : y;
    }
#endif
}

double hs_simdy_maximumDouble(double x, double y)
{
#if defined(__aarch64__)
    float64x1_t xx = vld1_f64(&x);
    float64x1_t yy = vld1_f64(&y);
    float64x1_t zz = vmax_f64(xx, yy);
    double result;
    vst1_f64(&result, zz);
    return result;
    /*
    double result;
    asm("fmax %d0, %d1, %d2" : "=w"(result) : "w"(x), "w"(y));
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
        return x + y; // propagate NaN
    } else {
        // equal or both zero
        return signbit(x) ? y : x;
    }
#endif
}
