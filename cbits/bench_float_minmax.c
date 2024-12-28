#include <fenv.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#if defined(__SSE2__)
#include <xmmintrin.h>
#include <immintrin.h>
#elif defined(__aarch64__)
#include <arm_neon.h>
#endif

#pragma STDC FENV_ACCESS ON

extern float hs_simdy_minimum_float(float x, float y);
extern float hs_simdy_maximum_float(float x, float y);
extern float hs_simdy_minimumNumber_float(float x, float y);
extern float hs_simdy_maximumNumber_float(float x, float y);
#if defined(__SSE2__)
extern __m128 hs_simdy_minimum_floatx4(__m128 x, __m128 y);
extern __m128 hs_simdy_maximum_floatx4(__m128 x, __m128 y);
extern __m128 hs_simdy_minimumNumber_floatx4(__m128 x, __m128 y);
extern __m128 hs_simdy_maximumNumber_floatx4(__m128 x, __m128 y);
__attribute__((target("avx")))
extern __m256 hs_simdy_minimum_floatx8(__m256 x, __m256 y);
__attribute__((target("avx")))
extern __m256 hs_simdy_maximum_floatx8(__m256 x, __m256 y);
__attribute__((target("avx")))
extern __m256 hs_simdy_minimumNumber_floatx8(__m256 x, __m256 y);
__attribute__((target("avx")))
extern __m256 hs_simdy_maximumNumber_floatx8(__m256 x, __m256 y);
__attribute__((target("avx512dq,avx512vl")))
extern __m128 hs_simdy_minimum_floatx4_avx512(__m128 x, __m128 y);
__attribute__((target("avx512dq,avx512vl")))
extern __m128 hs_simdy_maximum_floatx4_avx512(__m128 x, __m128 y);
__attribute__((target("avx512dq,avx512vl")))
extern __m128 hs_simdy_minimumNumber_floatx4_avx512(__m128 x, __m128 y);
__attribute__((target("avx512dq,avx512vl")))
extern __m128 hs_simdy_maximumNumber_floatx4_avx512(__m128 x, __m128 y);
__attribute__((target("avx512dq,avx512vl")))
extern __m256 hs_simdy_minimum_floatx8_avx512(__m256 x, __m256 y);
__attribute__((target("avx512dq,avx512vl")))
extern __m256 hs_simdy_maximum_floatx8_avx512(__m256 x, __m256 y);
__attribute__((target("avx512dq,avx512vl")))
extern __m256 hs_simdy_minimumNumber_floatx8_avx512(__m256 x, __m256 y);
__attribute__((target("avx512dq,avx512vl")))
extern __m256 hs_simdy_maximumNumber_floatx8_avx512(__m256 x, __m256 y);
__attribute__((target("avx512dq")))
extern __m512 hs_simdy_minimum_floatx16(__m512 x, __m512 y);
__attribute__((target("avx512dq")))
extern __m512 hs_simdy_maximum_floatx16(__m512 x, __m512 y);
__attribute__((target("avx512dq")))
extern __m512 hs_simdy_minimumNumber_floatx16(__m512 x, __m512 y);
__attribute__((target("avx512dq")))
extern __m512 hs_simdy_maximumNumber_floatx16(__m512 x, __m512 y);
#elif defined(__aarch64__)
float32x4_t hs_simdy_minimum_floatx4(float32x4_t x, float32x4_t y);
float32x4_t hs_simdy_maximum_floatx4(float32x4_t x, float32x4_t y);
float32x4_t hs_simdy_minimumNumber_floatx4(float32x4_t x, float32x4_t y);
float32x4_t hs_simdy_maximumNumber_floatx4(float32x4_t x, float32x4_t y);
#endif

void minimum_float_array_portable(size_t n, const float x[], const float y[], float result[restrict])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = hs_simdy_minimum_float(x[i], y[i]);
    }
}

void maximum_float_array_portable(size_t n, const float x[], const float y[], float result[restrict])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = hs_simdy_maximum_float(x[i], y[i]);
    }
}

void minimumNumber_float_array_portable(size_t n, const float x[], const float y[], float result[restrict])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = hs_simdy_minimumNumber_float(x[i], y[i]);
    }
}

void maximumNumber_float_array_portable(size_t n, const float x[], const float y[], float result[restrict])
{
    for (size_t i = 0; i < n; ++i) {
        result[i] = hs_simdy_maximumNumber_float(x[i], y[i]);
    }
}

#if defined(__SSE2__)
void minimum_float_array_sse(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        __m128 xx = _mm_loadu_ps(&x[i]);
        __m128 yy = _mm_loadu_ps(&y[i]);
        __m128 zz = hs_simdy_minimum_floatx4(xx, yy);
        _mm_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimum_float(x[i], y[i]);
    }
}
void maximum_float_array_sse(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        __m128 xx = _mm_loadu_ps(&x[i]);
        __m128 yy = _mm_loadu_ps(&y[i]);
        __m128 zz = hs_simdy_maximum_floatx4(xx, yy);
        _mm_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximum_float(x[i], y[i]);
    }
}
void minimumNumber_float_array_sse(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        __m128 xx = _mm_loadu_ps(&x[i]);
        __m128 yy = _mm_loadu_ps(&y[i]);
        __m128 zz = hs_simdy_minimumNumber_floatx4(xx, yy);
        _mm_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimumNumber_float(x[i], y[i]);
    }
}
void maximumNumber_float_array_sse(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        __m128 xx = _mm_loadu_ps(&x[i]);
        __m128 yy = _mm_loadu_ps(&y[i]);
        __m128 zz = hs_simdy_maximumNumber_floatx4(xx, yy);
        _mm_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximumNumber_float(x[i], y[i]);
    }
}
#endif

#if defined(__AVX__)
void minimum_float_array_avx(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 7 < n; i += 8) {
        __m256 xx = _mm256_loadu_ps(&x[i]);
        __m256 yy = _mm256_loadu_ps(&y[i]);
        __m256 zz = hs_simdy_minimum_floatx8(xx, yy);
        _mm256_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimum_float(x[i], y[i]);
    }
}
void maximum_float_array_avx(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 7 < n; i += 8) {
        __m256 xx = _mm256_loadu_ps(&x[i]);
        __m256 yy = _mm256_loadu_ps(&y[i]);
        __m256 zz = hs_simdy_maximum_floatx8(xx, yy);
        _mm256_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximum_float(x[i], y[i]);
    }
}
void minimumNumber_float_array_avx(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 7 < n; i += 8) {
        __m256 xx = _mm256_loadu_ps(&x[i]);
        __m256 yy = _mm256_loadu_ps(&y[i]);
        __m256 zz = hs_simdy_minimumNumber_floatx8(xx, yy);
        _mm256_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimumNumber_float(x[i], y[i]);
    }
}
void maximumNumber_float_array_avx(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 7 < n; i += 8) {
        __m256 xx = _mm256_loadu_ps(&x[i]);
        __m256 yy = _mm256_loadu_ps(&y[i]);
        __m256 zz = hs_simdy_maximumNumber_floatx8(xx, yy);
        _mm256_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximumNumber_float(x[i], y[i]);
    }
}
#endif

#if defined(__AVX512DQ__) && defined(__AVX512VL__)
void minimum_float_array_avx512_x4(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        __m128 xx = _mm_loadu_ps(&x[i]);
        __m128 yy = _mm_loadu_ps(&y[i]);
        __m128 zz = hs_simdy_minimum_floatx4_avx512(xx, yy);
        _mm_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimum_float(x[i], y[i]);
    }
}
void maximum_float_array_avx512_x4(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        __m128 xx = _mm_loadu_ps(&x[i]);
        __m128 yy = _mm_loadu_ps(&y[i]);
        __m128 zz = hs_simdy_maximum_floatx4_avx512(xx, yy);
        _mm_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximum_float(x[i], y[i]);
    }
}
void minimumNumber_float_array_avx512_x4(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        __m128 xx = _mm_loadu_ps(&x[i]);
        __m128 yy = _mm_loadu_ps(&y[i]);
        __m128 zz = hs_simdy_minimumNumber_floatx4_avx512(xx, yy);
        _mm_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimumNumber_float(x[i], y[i]);
    }
}
void maximumNumber_float_array_avx512_x4(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        __m128 xx = _mm_loadu_ps(&x[i]);
        __m128 yy = _mm_loadu_ps(&y[i]);
        __m128 zz = hs_simdy_maximumNumber_floatx4_avx512(xx, yy);
        _mm_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximumNumber_float(x[i], y[i]);
    }
}
void minimum_float_array_avx512_x8(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 7 < n; i += 8) {
        __m256 xx = _mm256_loadu_ps(&x[i]);
        __m256 yy = _mm256_loadu_ps(&y[i]);
        __m256 zz = hs_simdy_minimum_floatx8_avx512(xx, yy);
        _mm256_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimum_float(x[i], y[i]);
    }
}
void maximum_float_array_avx512_x8(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 7 < n; i += 8) {
        __m256 xx = _mm256_loadu_ps(&x[i]);
        __m256 yy = _mm256_loadu_ps(&y[i]);
        __m256 zz = hs_simdy_maximum_floatx8_avx512(xx, yy);
        _mm256_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximum_float(x[i], y[i]);
    }
}
void minimumNumber_float_array_avx512_x8(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 7 < n; i += 8) {
        __m256 xx = _mm256_loadu_ps(&x[i]);
        __m256 yy = _mm256_loadu_ps(&y[i]);
        __m256 zz = hs_simdy_minimumNumber_floatx8_avx512(xx, yy);
        _mm256_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimumNumber_float(x[i], y[i]);
    }
}
void maximumNumber_float_array_avx512_x8(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 7 < n; i += 8) {
        __m256 xx = _mm256_loadu_ps(&x[i]);
        __m256 yy = _mm256_loadu_ps(&y[i]);
        __m256 zz = hs_simdy_maximumNumber_floatx8_avx512(xx, yy);
        _mm256_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximumNumber_float(x[i], y[i]);
    }
}

void minimum_float_array_avx512_x16(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 15 < n; i += 16) {
        __m512 xx = _mm512_loadu_ps(&x[i]);
        __m512 yy = _mm512_loadu_ps(&y[i]);
        __m512 zz = hs_simdy_minimum_floatx16(xx, yy);
        _mm512_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimum_float(x[i], y[i]);
    }
}
void maximum_float_array_avx512_x16(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 15 < n; i += 16) {
        __m512 xx = _mm512_loadu_ps(&x[i]);
        __m512 yy = _mm512_loadu_ps(&y[i]);
        __m512 zz = hs_simdy_maximum_floatx16(xx, yy);
        _mm512_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximum_float(x[i], y[i]);
    }
}
void minimumNumber_float_array_avx512_x16(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 15 < n; i += 16) {
        __m512 xx = _mm512_loadu_ps(&x[i]);
        __m512 yy = _mm512_loadu_ps(&y[i]);
        __m512 zz = hs_simdy_minimumNumber_floatx16(xx, yy);
        _mm512_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimumNumber_float(x[i], y[i]);
    }
}
void maximumNumber_float_array_avx512_x16(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 15 < n; i += 16) {
        __m512 xx = _mm512_loadu_ps(&x[i]);
        __m512 yy = _mm512_loadu_ps(&y[i]);
        __m512 zz = hs_simdy_maximumNumber_floatx16(xx, yy);
        _mm512_storeu_ps(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximumNumber_float(x[i], y[i]);
    }
}
#endif

#if defined(__aarch64__)
void minimum_float_array_neon(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        float32x4_t xx = vld1q_f32(&x[i]);
        float32x4_t yy = vld1q_f32(&y[i]);
        float32x4_t zz = hs_simdy_minimum_floatx4(xx, yy);
        vst1q_f32(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimum_float(x[i], y[i]);
    }
}
void maximum_float_array_neon(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        float32x4_t xx = vld1q_f32(&x[i]);
        float32x4_t yy = vld1q_f32(&y[i]);
        float32x4_t zz = hs_simdy_maximum_floatx4(xx, yy);
        vst1q_f32(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximum_float(x[i], y[i]);
    }
}
void minimumNumber_float_array_neon(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        float32x4_t xx = vld1q_f32(&x[i]);
        float32x4_t yy = vld1q_f32(&y[i]);
        float32x4_t zz = hs_simdy_minimumNumber_floatx4(xx, yy);
        vst1q_f32(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_minimumNumber_float(x[i], y[i]);
    }
}
void maximumNumber_float_array_neon(size_t n, const float x[], const float y[], float result[restrict])
{
    size_t i;
    for (i = 0; i + 3 < n; i += 4) {
        float32x4_t xx = vld1q_f32(&x[i]);
        float32x4_t yy = vld1q_f32(&y[i]);
        float32x4_t zz = hs_simdy_maximumNumber_floatx4(xx, yy);
        vst1q_f32(&result[i], zz);
    }
    for (; i < n; ++i) {
        result[i] = hs_simdy_maximumNumber_float(x[i], y[i]);
    }
}

#endif

void bench(const char *name, void (*f)(size_t n, const float x[], const float y[], float result[restrict]))
{
    const size_t M = 10000;
    float *x = malloc(sizeof(float) * M);
    float *y = malloc(sizeof(float) * M);
    float *result = malloc(sizeof(float) * M);
    for (size_t i = 0; i < M; ++i) {
        switch (i % 5) {
        case 0:
            x[i] = 1.0;
            y[i] = 2.0;
            break;
        case 1:
            x[i] = 0.0;
            y[i] = -0.0;
            break;
        case 2:
            x[i] = 4.0;
            y[i] = -5.0;
            break;
        case 3:
            x[i] = 0.0;
            y[i] = 0.0;
            break;
        case 4:
            x[i] = 7.0;
            y[i] = 7.0;
            break;
        }
    }
    const size_t N = 100000;
    clock_t start = clock();
    for (size_t i = 0; i < N; ++i) {
        f(M, x, y, result);
    }
    clock_t end = clock();
    printf("numeric %s: %.03f s\n", name, (double)(end - start) / CLOCKS_PER_SEC);
    free(x);
    free(y);
    free(result);
}

void bench_nan(const char *name, void (*f)(size_t n, const float x[], const float y[], float result[restrict]))
{
    const size_t M = 10000;
    float *x = malloc(sizeof(float) * M);
    float *y = malloc(sizeof(float) * M);
    float *result = malloc(sizeof(float) * M);
    for (size_t i = 0; i < M; ++i) {
        switch (i % 4) {
        case 0:
            x[i] = NAN;
            y[i] = 2.0;
            break;
        case 1:
            x[i] = 0.0;
            y[i] = -0.0;
            break;
        case 2:
            x[i] = 4.0;
            y[i] = -5.0;
            break;
        case 3:
            x[i] = 0.0;
            y[i] = NAN;
            break;
        }
    }
    const size_t N = 100000;
    clock_t start = clock();
    for (size_t i = 0; i < N; ++i) {
        f(M, x, y, result);
    }
    clock_t end = clock();
    printf("NaN %s: %.03f s\n", name, (double)(end - start) / CLOCKS_PER_SEC);
    free(x);
    free(y);
    free(result);
}

int main(int argc, char *argv[])
{
    bench("minimum/portable", minimum_float_array_portable);
#if defined(__SSE2__)
    bench("minimum/SSE", minimum_float_array_sse);
#endif
#if defined(__AVX__)
    bench("minimum/AVX", minimum_float_array_avx);
#endif
#if defined(__AVX512DQ__) && defined(__AVX512VL__)
    bench("minimum/AVX-512/x4", minimum_float_array_avx512_x4);
    bench("minimum/AVX-512/x8", minimum_float_array_avx512_x8);
    bench("minimum/AVX-512/x16", minimum_float_array_avx512_x16);
#endif
#if defined(__aarch64__)
    bench("minimum/NEON", minimum_float_array_neon);
#endif

    bench_nan("minimum/portable", minimum_float_array_portable);
#if defined(__SSE2__)
    bench_nan("minimum/SSE", minimum_float_array_sse);
#endif
#if defined(__AVX__)
    bench_nan("minimum/AVX", minimum_float_array_avx);
#endif
#if defined(__AVX512DQ__) && defined(__AVX512VL__)
    bench_nan("minimum/AVX-512/x4", minimum_float_array_avx512_x4);
    bench_nan("minimum/AVX-512/x8", minimum_float_array_avx512_x8);
    bench_nan("minimum/AVX-512/x16", minimum_float_array_avx512_x16);
#endif
#if defined(__aarch64__)
    bench_nan("minimum/NEON", minimum_float_array_neon);
#endif

    bench("minimumNumber/portable", minimumNumber_float_array_portable);
#if defined(__SSE2__)
    bench("minimumNumber/SSE", minimumNumber_float_array_sse);
#endif
#if defined(__AVX__)
    bench("minimumNumber/AVX", minimumNumber_float_array_avx);
#endif
#if defined(__AVX512DQ__) && defined(__AVX512VL__)
    bench("minimumNumber/AVX-512/x4", minimumNumber_float_array_avx512_x4);
    bench("minimumNumber/AVX-512/x8", minimumNumber_float_array_avx512_x8);
    bench("minimumNumber/AVX-512/x16", minimumNumber_float_array_avx512_x16);
#endif
#if defined(__aarch64__)
    bench("minimumNumber/NEON", minimumNumber_float_array_neon);
#endif

    bench_nan("minimumNumber/portable", minimumNumber_float_array_portable);
#if defined(__SSE2__)
    bench_nan("minimumNumber/SSE", minimumNumber_float_array_sse);
#endif
#if defined(__AVX__)
    bench_nan("minimumNumber/AVX", minimumNumber_float_array_avx);
#endif
#if defined(__AVX512DQ__) && defined(__AVX512VL__)
    bench_nan("minimumNumber/AVX-512/x4", minimumNumber_float_array_avx512_x4);
    bench_nan("minimumNumber/AVX-512/x8", minimumNumber_float_array_avx512_x8);
    bench_nan("minimumNumber/AVX-512/x16", minimumNumber_float_array_avx512_x16);
#endif
#if defined(__aarch64__)
    bench_nan("minimumNumber/NEON", minimumNumber_float_array_neon);
#endif
}
