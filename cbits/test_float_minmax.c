#include <fenv.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined(__SSE2__)
#include <xmmintrin.h>
#include <immintrin.h>
#elif defined(__aarch64__)
#include <arm_neon.h>
#endif

#pragma STDC FENV_ACCESS ON

extern float hs_simdy_minimumFloat(float x, float y);
extern float hs_simdy_maximumFloat(float x, float y);
extern float hs_simdy_minimumNumberFloat(float x, float y);
extern float hs_simdy_maximumNumberFloat(float x, float y);
#if defined(__SSE2__)
extern __m128 hs_simdy_minimumFloatX4(__m128 x, __m128 y);
extern __m128 hs_simdy_maximumFloatX4(__m128 x, __m128 y);
extern __m128 hs_simdy_minimumNumberFloatX4(__m128 x, __m128 y);
extern __m128 hs_simdy_maximumNumberFloatX4(__m128 x, __m128 y);
#if defined(__AVX__)
extern __m256 hs_simdy_minimumFloatX8(__m256 x, __m256 y);
extern __m256 hs_simdy_maximumFloatX8(__m256 x, __m256 y);
extern __m256 hs_simdy_minimumNumberFloatX8(__m256 x, __m256 y);
extern __m256 hs_simdy_maximumNumberFloatX8(__m256 x, __m256 y);
#endif
#if defined(__AVX512DQ__) && defined(__AVX512VL__)
extern __m128 hs_simdy_minimumFloatX4_avx512(__m128 x, __m128 y);
extern __m128 hs_simdy_maximumFloatX4_avx512(__m128 x, __m128 y);
extern __m128 hs_simdy_minimumNumberFloatX4_avx512(__m128 x, __m128 y);
extern __m128 hs_simdy_maximumNumberFloatX4_avx512(__m128 x, __m128 y);
extern __m256 hs_simdy_minimumFloatX8_avx512(__m256 x, __m256 y);
extern __m256 hs_simdy_maximumFloatX8_avx512(__m256 x, __m256 y);
extern __m256 hs_simdy_minimumNumberFloatX8_avx512(__m256 x, __m256 y);
extern __m256 hs_simdy_maximumNumberFloatX8_avx512(__m256 x, __m256 y);
#endif
#if defined(__AVX512DQ__)
extern __m512 hs_simdy_minimumFloatX16(__m512 x, __m512 y);
extern __m512 hs_simdy_maximumFloatX16(__m512 x, __m512 y);
extern __m512 hs_simdy_minimumNumberFloatX16(__m512 x, __m512 y);
extern __m512 hs_simdy_maximumNumberFloatX16(__m512 x, __m512 y);
#endif
#elif defined(__aarch64__)
float32x4_t hs_simdy_minimumFloatX4(float32x4_t x, float32x4_t y);
float32x4_t hs_simdy_maximumFloatX4(float32x4_t x, float32x4_t y);
float32x4_t hs_simdy_minimumNumberFloatX4(float32x4_t x, float32x4_t y);
float32x4_t hs_simdy_maximumNumberFloatX4(float32x4_t x, float32x4_t y);
#endif

void minimum_float_array_4_portable(const float x[4], const float y[4], float result[restrict 4])
{
    for (int i = 0; i < 4; ++i) {
        result[i] = hs_simdy_minimumFloat(x[i], y[i]);
    }
}

void maximum_float_array_4_portable(const float x[4], const float y[4], float result[restrict 4])
{
    for (int i = 0; i < 4; ++i) {
        result[i] = hs_simdy_maximumFloat(x[i], y[i]);
    }
}

void minimumNumber_float_array_4_portable(const float x[4], const float y[4], float result[restrict 4])
{
    for (int i = 0; i < 4; ++i) {
        result[i] = hs_simdy_minimumNumberFloat(x[i], y[i]);
    }
}

void maximumNumber_float_array_4_portable(const float x[4], const float y[4], float result[restrict 4])
{
    for (int i = 0; i < 4; ++i) {
        result[i] = hs_simdy_maximumNumberFloat(x[i], y[i]);
    }
}

#if defined(__SSE2__)
void minimum_float_array_4_sse(const float x[4], const float y[4], float result[restrict 4])
{
    __m128 xx = _mm_loadu_ps(x);
    __m128 yy = _mm_loadu_ps(y);
    __m128 zz = hs_simdy_minimumFloatX4(xx, yy);
    _mm_storeu_ps(result, zz);
}
void maximum_float_array_4_sse(const float x[4], const float y[4], float result[restrict 4])
{
    __m128 xx = _mm_loadu_ps(x);
    __m128 yy = _mm_loadu_ps(y);
    __m128 zz = hs_simdy_maximumFloatX4(xx, yy);
    _mm_storeu_ps(result, zz);
}
void minimumNumber_float_array_4_sse(const float x[4], const float y[4], float result[restrict 4])
{
    __m128 xx = _mm_loadu_ps(x);
    __m128 yy = _mm_loadu_ps(y);
    __m128 zz = hs_simdy_minimumNumberFloatX4(xx, yy);
    _mm_storeu_ps(result, zz);
}
void maximumNumber_float_array_4_sse(const float x[4], const float y[4], float result[restrict 4])
{
    __m128 xx = _mm_loadu_ps(x);
    __m128 yy = _mm_loadu_ps(y);
    __m128 zz = hs_simdy_maximumNumberFloatX4(xx, yy);
    _mm_storeu_ps(result, zz);
}
#endif

#if defined(__AVX__)
void minimum_float_array_8_avx(const float x[8], const float y[8], float result[restrict 8])
{
    __m256 xx = _mm256_loadu_ps(x);
    __m256 yy = _mm256_loadu_ps(y);
    __m256 zz = hs_simdy_minimumFloatX8(xx, yy);
    _mm256_storeu_ps(result, zz);
}
void maximum_float_array_8_avx(const float x[8], const float y[8], float result[restrict 8])
{
    __m256 xx = _mm256_loadu_ps(x);
    __m256 yy = _mm256_loadu_ps(y);
    __m256 zz = hs_simdy_maximumFloatX8(xx, yy);
    _mm256_storeu_ps(result, zz);
}
void minimumNumber_float_array_8_avx(const float x[8], const float y[8], float result[restrict 8])
{
    __m256 xx = _mm256_loadu_ps(x);
    __m256 yy = _mm256_loadu_ps(y);
    __m256 zz = hs_simdy_minimumNumberFloatX8(xx, yy);
    _mm256_storeu_ps(result, zz);
}
void maximumNumber_float_array_8_avx(const float x[8], const float y[8], float result[restrict 8])
{
    __m256 xx = _mm256_loadu_ps(x);
    __m256 yy = _mm256_loadu_ps(y);
    __m256 zz = hs_simdy_maximumNumberFloatX8(xx, yy);
    _mm256_storeu_ps(result, zz);
}
#endif

#if defined(__AVX512DQ__) && defined(__AVX512VL__)
void minimum_float_array_4_avx512(const float x[4], const float y[4], float result[restrict 4])
{
    __m128 xx = _mm_loadu_ps(x);
    __m128 yy = _mm_loadu_ps(y);
    __m128 zz = hs_simdy_minimumFloatX4_avx512(xx, yy);
    _mm_storeu_ps(result, zz);
}
void maximum_float_array_4_avx512(const float x[4], const float y[4], float result[restrict 4])
{
    __m128 xx = _mm_loadu_ps(x);
    __m128 yy = _mm_loadu_ps(y);
    __m128 zz = hs_simdy_maximumFloatX4_avx512(xx, yy);
    _mm_storeu_ps(result, zz);
}
void minimumNumber_float_array_4_avx512(const float x[4], const float y[4], float result[restrict 4])
{
    __m128 xx = _mm_loadu_ps(x);
    __m128 yy = _mm_loadu_ps(y);
    __m128 zz = hs_simdy_minimumNumberFloatX4_avx512(xx, yy);
    _mm_storeu_ps(result, zz);
}
void maximumNumber_float_array_4_avx512(const float x[4], const float y[4], float result[restrict 4])
{
    __m128 xx = _mm_loadu_ps(x);
    __m128 yy = _mm_loadu_ps(y);
    __m128 zz = hs_simdy_maximumNumberFloatX4_avx512(xx, yy);
    _mm_storeu_ps(result, zz);
}
void minimum_float_array_8_avx512(const float x[8], const float y[8], float result[restrict 8])
{
    __m256 xx = _mm256_loadu_ps(x);
    __m256 yy = _mm256_loadu_ps(y);
    __m256 zz = hs_simdy_minimumFloatX8_avx512(xx, yy);
    _mm256_storeu_ps(result, zz);
}
void maximum_float_array_8_avx512(const float x[8], const float y[8], float result[restrict 8])
{
    __m256 xx = _mm256_loadu_ps(x);
    __m256 yy = _mm256_loadu_ps(y);
    __m256 zz = hs_simdy_maximumFloatX8_avx512(xx, yy);
    _mm256_storeu_ps(result, zz);
}
void minimumNumber_float_array_8_avx512(const float x[8], const float y[8], float result[restrict 8])
{
    __m256 xx = _mm256_loadu_ps(x);
    __m256 yy = _mm256_loadu_ps(y);
    __m256 zz = hs_simdy_minimumNumberFloatX8_avx512(xx, yy);
    _mm256_storeu_ps(result, zz);
}
void maximumNumber_float_array_8_avx512(const float x[8], const float y[8], float result[restrict 8])
{
    __m256 xx = _mm256_loadu_ps(x);
    __m256 yy = _mm256_loadu_ps(y);
    __m256 zz = hs_simdy_maximumNumberFloatX8_avx512(xx, yy);
    _mm256_storeu_ps(result, zz);
}
void minimum_float_array_16_avx512(const float x[16], const float y[16], float result[restrict 16])
{
    __m512 xx = _mm512_loadu_ps(x);
    __m512 yy = _mm512_loadu_ps(y);
    __m512 zz = hs_simdy_minimumFloatX16(xx, yy);
    _mm512_storeu_ps(result, zz);
}
void maximum_float_array_16_avx512(const float x[16], const float y[16], float result[restrict 16])
{
    __m512 xx = _mm512_loadu_ps(x);
    __m512 yy = _mm512_loadu_ps(y);
    __m512 zz = hs_simdy_maximumFloatX16(xx, yy);
    _mm512_storeu_ps(result, zz);
}
void minimumNumber_float_array_16_avx512(const float x[16], const float y[16], float result[restrict 16])
{
    __m512 xx = _mm512_loadu_ps(x);
    __m512 yy = _mm512_loadu_ps(y);
    __m512 zz = hs_simdy_minimumNumberFloatX16(xx, yy);
    _mm512_storeu_ps(result, zz);
}
void maximumNumber_float_array_16_avx512(const float x[16], const float y[16], float result[restrict 16])
{
    __m512 xx = _mm512_loadu_ps(x);
    __m512 yy = _mm512_loadu_ps(y);
    __m512 zz = hs_simdy_maximumNumberFloatX16(xx, yy);
    _mm512_storeu_ps(result, zz);
}
#endif

#if defined(__aarch64__)
void minimum_float_array_4_neon(const float x[4], const float y[4], float result[restrict 4])
{
    float32x4_t xx = vld1q_f32(x);
    float32x4_t yy = vld1q_f32(y);
    float32x4_t zz = hs_simdy_minimumFloatX4(xx, yy);
    vst1q_f32(result, zz);
}
void maximum_float_array_4_neon(const float x[4], const float y[4], float result[restrict 4])
{
    float32x4_t xx = vld1q_f32(x);
    float32x4_t yy = vld1q_f32(y);
    float32x4_t zz = hs_simdy_maximumFloatX4(xx, yy);
    vst1q_f32(result, zz);
}
void minimumNumber_float_array_4_neon(const float x[4], const float y[4], float result[restrict 4])
{
    float32x4_t xx = vld1q_f32(x);
    float32x4_t yy = vld1q_f32(y);
    float32x4_t zz = hs_simdy_minimumNumberFloatX4(xx, yy);
    vst1q_f32(result, zz);
}
void maximumNumber_float_array_4_neon(const float x[4], const float y[4], float result[restrict 4])
{
    float32x4_t xx = vld1q_f32(x);
    float32x4_t yy = vld1q_f32(y);
    float32x4_t zz = hs_simdy_maximumNumberFloatX4(xx, yy);
    vst1q_f32(result, zz);
}
#endif

bool my_issignaling(float x)
{
    uint32_t i;
    memcpy(&i, &x, 4);
    return (i & 0x7FC00000) == 0x7F800000 && (i & 0x7FFFFFFF) != 0x7F800000;
}

bool same_float(float x, float y)
{
    if (isnan(x) && isnan(y)) {
        return my_issignaling(x) == my_issignaling(y);
    } else {
        return x == y && signbit(x) == signbit(y);
    }
}

void print_float(float x)
{
    if (my_issignaling(x)) {
        printf("sNaN");
    } else if (isnan(x)) {
        printf("qNaN");
    } else {
        printf("%g", x);
    }
}

void print_exception(int e)
{
    bool sep = false;
    if (e & FE_DIVBYZERO) {
        printf("DIVBYZERO");
        sep = true;
    }
    if (e & FE_INEXACT) {
        if (sep) {
            putchar('+');
        }
        printf("INEXACT");
        sep = true;
    }
    if (e & FE_INVALID) {
        if (sep) {
            putchar('+');
        }
        printf("INVALID");
        sep = true;
    }
    if (e & FE_OVERFLOW) {
        if (sep) {
            putchar('+');
        }
        printf("OVERFLOW");
        sep = true;
    }
    if (e & FE_UNDERFLOW) {
        if (sep) {
            putchar('+');
        }
        printf("UNDERFLOW");
        sep = true;
    }
    if (e == 0) {
        printf("no exception");
    } else if (!sep) {
        printf("unknown exception");
    }
}

struct test_data
{
    float x;
    float y;
    float min;
    float max;
    float minnm;
    float maxnm;
    bool invalid;
} test_data[] = {
    {/* SNAN */ 0.0f, NAN, NAN, NAN, NAN, NAN, true},
    {NAN, /* SNAN */ 0.0f, NAN, NAN, NAN, NAN, true},
    {/* SNAN */ 0.0f, 5.0f, NAN, NAN, 5.0f, 5.0f, true},
    {-0.0f, /* SNAN */ 0.0f, NAN, NAN, -0.0f, -0.0f, true},
    {/* SNAN */ 0.0f, /* SNAN */ 0.0f, NAN, NAN, NAN, NAN, true},
    {1.0f, 2.0f, 1.0f, 2.0f, 1.0f, 2.0f, false},
    {5.0f, 2.0f, 2.0f, 5.0f, 2.0f, 5.0f, false},
    {5.0f, 0.0f, 0.0f, 5.0f, 0.0f, 5.0f, false},
    {0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, false},
    {0.0f, -0.0f, -0.0f, 0.0f, -0.0f, 0.0f, false},
    {-0.0f, 0.0f, -0.0f, 0.0f, -0.0f, 0.0f, false},
    {-0.0f, -0.0f, -0.0f, -0.0f, -0.0f, -0.0f, false},
    {INFINITY, -INFINITY, -INFINITY, INFINITY, -INFINITY, INFINITY, false},
    {-333.0f, -INFINITY, -INFINITY, -333.0f, -INFINITY, -333.0f, false},
    {-0.0f, INFINITY, -0.0f, INFINITY, -0.0f, INFINITY, false},
    {INFINITY, NAN, NAN, NAN, INFINITY, INFINITY, false},
    {NAN, 0.0f, NAN, NAN, 0.0f, 0.0f, false},
    {3.0f, NAN, NAN, NAN, 3.0f, 3.0f, false},
    {NAN, NAN, NAN, NAN, NAN, NAN, false},
};

void set_snan(float *result)
{
    uint32_t i = 0x7F800001;
    memcpy(result, &i, 4);
}

void test_commutativity(const char *name, float (*f)(float, float))
{
    for (int i = 0; i < sizeof(test_data) / sizeof(test_data[0]); ++i) {
        float x = test_data[i].x;
        float y = test_data[i].y;
        feclearexcept(FE_ALL_EXCEPT);
        float result1 = f(x, y);
        int excepts1 = fetestexcept(FE_ALL_EXCEPT);
        feclearexcept(FE_ALL_EXCEPT);
        float result2 = f(y, x);
        int excepts2 = fetestexcept(FE_ALL_EXCEPT);
        bool ok = same_float(result1, result2) && excepts1 == excepts2;
        if (!ok) {
            printf("%s(", name);
            print_float(x);
            printf(", ");
            print_float(y);
            printf(") != %s", name);
            print_float(y);
            printf(", ");
            print_float(x);
            printf(")\n");
            exit(1);
        }
    }
    printf("commutativity for %s...OK\n", name);
}

void test_scalar(const char *name, float (*f)(float, float), size_t offset)
{
    for (int i = 0; i < sizeof(test_data) / sizeof(test_data[0]); ++i) {
        float x = test_data[i].x;
        float y = test_data[i].y;
        float expected = *(const float *)((const char *)&test_data[i] + offset);
        bool invalid = test_data[i].invalid;
        feclearexcept(FE_ALL_EXCEPT);
        float result = f(x, y);
        int excepts = fetestexcept(FE_ALL_EXCEPT);
        bool ok = same_float(result, expected) && excepts == (invalid ? FE_INVALID : 0);
        printf("%s(", name);
        print_float(x);
        printf(", ");
        print_float(y);
        printf(") = ");
        print_float(result);
        printf(", with ");
        print_exception(excepts);
        if (ok) {
            printf("...OK\n");
        } else {
            printf("...expected ");
            print_float(expected);
            printf(" with %s\n", invalid ? "INVALID" : "no exception");
            exit(1);
        }
    }
}

void test_array(size_t n, const char *name, void (*f_arr)(const float [], const float [], float [restrict]), size_t offset)
{
    for (int i = 0; i < sizeof(test_data) / sizeof(test_data[0]); ++i) {
        float x = test_data[i].x;
        float y = test_data[i].y;
        float expected = *(const float *)((const char *)&test_data[i] + offset);
        bool invalid = test_data[i].invalid;
        float xarr[n];
        float yarr[n];
        float resultarr[n];
        for (int j = 0; j < n; ++j) {
            xarr[j] = x;
            yarr[j] = y;
        }
        feclearexcept(FE_ALL_EXCEPT);
        f_arr(xarr, yarr, resultarr);
        int excepts = fetestexcept(FE_ALL_EXCEPT);
        bool ok = excepts == (invalid ? FE_INVALID : 0);
        for (int j = 0; j < n; ++j) {
            ok = ok && same_float(resultarr[j], expected);
        }
        printf("%s(", name);
        print_float(x);
        printf(", ");
        print_float(y);
        printf(") = ");
        print_float(resultarr[0]);
        printf(", with ");
        print_exception(excepts);
        if (ok) {
            printf("...OK\n");
        } else {
            printf("...expected ");
            print_float(expected);
            printf(" with %s\n", invalid ? "INVALID" : "no exception");
            exit(1);
        }
    }
    bool ok = true;
    for (int i0 = 0; i0 < sizeof(test_data) / sizeof(test_data[0]); ++i0) {
        float x0 = test_data[i0].x;
        float y0 = test_data[i0].y;
        float expected0 = *(const float *)((const char *)&test_data[i0] + offset);
        bool invalid0 = test_data[i0].invalid;
        for (int i1 = 0; i1 < sizeof(test_data) / sizeof(test_data[0]); ++i1) {
            float x1 = test_data[i1].x;
            float y1 = test_data[i1].y;
            float expected1 = *(const float *)((const char *)&test_data[i1] + offset);
            bool invalid1 = test_data[i1].invalid;
            float xarr[n];
            float yarr[n];
            float resultarr[n];
            for (int j = 0; j < n; ++j) {
                xarr[j] = j % 2 == 0 ? x0 : x1;
                yarr[j] = j % 2 == 0 ? y0 : y1;
            }
            feclearexcept(FE_ALL_EXCEPT);
            f_arr(xarr, yarr, resultarr);
            int excepts = fetestexcept(FE_ALL_EXCEPT);
            ok = ok && excepts == (invalid0 || invalid1 ? FE_INVALID : 0);
            for (int j = 0; j < n; ++j) {
                ok = ok && same_float(resultarr[j], j % 2 == 0 ? expected0 : expected1);
            }
        }
    }
    printf("%s...%s\n", name, ok ? "OK" : "failed");
    if (!ok) {
        exit(1);
    }
}

int main(int argc, char *argv[])
{
    set_snan(&test_data[0].x);
    set_snan(&test_data[1].y);
    set_snan(&test_data[2].x);
    set_snan(&test_data[3].y);
    set_snan(&test_data[4].x);
    set_snan(&test_data[4].y);
    test_scalar("hs_simdy_minimumFloat", hs_simdy_minimumFloat, offsetof(struct test_data, min));
    test_scalar("hs_simdy_maximumFloat", hs_simdy_maximumFloat, offsetof(struct test_data, max));
    test_scalar("hs_simdy_minimumNumberFloat", hs_simdy_minimumNumberFloat, offsetof(struct test_data, minnm));
    test_scalar("hs_simdy_maximumNumberFloat", hs_simdy_maximumNumberFloat, offsetof(struct test_data, maxnm));
    test_commutativity("hs_simdy_minimumFloat", hs_simdy_minimumFloat);
    test_commutativity("hs_simdy_maximumFloat", hs_simdy_maximumFloat);
    test_commutativity("hs_simdy_minimumNumberFloat", hs_simdy_minimumNumberFloat);
    test_commutativity("hs_simdy_maximumNumberFloat", hs_simdy_maximumNumberFloat);
    test_array(4, "minimum_float_array_4_portable", minimum_float_array_4_portable, offsetof(struct test_data, min));
    test_array(4, "maximum_float_array_4_portable", maximum_float_array_4_portable, offsetof(struct test_data, max));
    test_array(4, "minimumNumber_float_array_4_portable", minimumNumber_float_array_4_portable, offsetof(struct test_data, minnm));
    test_array(4, "maximumNumber_float_array_4_portable", maximumNumber_float_array_4_portable, offsetof(struct test_data, maxnm));
#if defined(__SSE2__)
    test_array(4, "minimum_float_array_4_sse", minimum_float_array_4_sse, offsetof(struct test_data, min));
    test_array(4, "maximum_float_array_4_sse", maximum_float_array_4_sse, offsetof(struct test_data, max));
    test_array(4, "minimumNumber_float_array_4_sse", minimumNumber_float_array_4_sse, offsetof(struct test_data, minnm));
    test_array(4, "maximumNumber_float_array_4_sse", maximumNumber_float_array_4_sse, offsetof(struct test_data, maxnm));
#endif
#if defined(__AVX__)
    test_array(8, "minimum_float_array_8_avx", minimum_float_array_8_avx, offsetof(struct test_data, min));
    test_array(8, "maximum_float_array_8_avx", maximum_float_array_8_avx, offsetof(struct test_data, max));
    test_array(8, "minimumNumber_float_array_8_avx", minimumNumber_float_array_8_avx, offsetof(struct test_data, minnm));
    test_array(8, "maximumNumber_float_array_8_avx", maximumNumber_float_array_8_avx, offsetof(struct test_data, maxnm));
#endif
#if defined(__AVX512DQ__) && defined(__AVX512VL__)
    test_array(4, "minimum_float_array_4_avx512", minimum_float_array_4_avx512, offsetof(struct test_data, min));
    test_array(4, "maximum_float_array_4_avx512", maximum_float_array_4_avx512, offsetof(struct test_data, max));
    test_array(4, "minimumNumber_float_array_4_avx512", minimumNumber_float_array_4_avx512, offsetof(struct test_data, minnm));
    test_array(4, "maximumNumber_float_array_4_avx512", maximumNumber_float_array_4_avx512, offsetof(struct test_data, maxnm));
    test_array(8, "minimum_float_array_8_avx512", minimum_float_array_8_avx512, offsetof(struct test_data, min));
    test_array(8, "maximum_float_array_8_avx512", maximum_float_array_8_avx512, offsetof(struct test_data, max));
    test_array(8, "minimumNumber_float_array_8_avx512", minimumNumber_float_array_8_avx512, offsetof(struct test_data, minnm));
    test_array(8, "maximumNumber_float_array_8_avx512", maximumNumber_float_array_8_avx512, offsetof(struct test_data, maxnm));
    test_array(16, "minimum_float_array_16_avx512", minimum_float_array_16_avx512, offsetof(struct test_data, min));
    test_array(16, "maximum_float_array_16_avx512", maximum_float_array_16_avx512, offsetof(struct test_data, max));
    test_array(16, "minimumNumber_float_array_16_avx512", minimumNumber_float_array_16_avx512, offsetof(struct test_data, minnm));
    test_array(16, "maximumNumber_float_array_16_avx512", maximumNumber_float_array_16_avx512, offsetof(struct test_data, maxnm));
#endif
#if defined(__aarch64__)
    test_array(4, "minimum_float_array_4_neon", minimum_float_array_4_neon, offsetof(struct test_data, min));
    test_array(4, "maximum_float_array_4_neon", maximum_float_array_4_neon, offsetof(struct test_data, max));
    test_array(4, "minimumNumber_float_array_4_neon", minimumNumber_float_array_4_neon, offsetof(struct test_data, minnm));
    test_array(4, "maximumNumber_float_array_4_neon", maximumNumber_float_array_4_neon, offsetof(struct test_data, maxnm));
#endif
}
