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

extern double hs_simdy_minimum_double(double x, double y);
extern double hs_simdy_maximum_double(double x, double y);
extern double hs_simdy_minimumNumber_double(double x, double y);
extern double hs_simdy_maximumNumber_double(double x, double y);
#if defined(__SSE2__)
extern __m128d hs_simdy_minimum_doublex2(__m128d x, __m128d y);
extern __m128d hs_simdy_maximum_doublex2(__m128d x, __m128d y);
extern __m128d hs_simdy_minimumNumber_doublex2(__m128d x, __m128d y);
extern __m128d hs_simdy_maximumNumber_doublex2(__m128d x, __m128d y);
#if defined(__AVX__)
extern __m256d hs_simdy_minimum_doublex4(__m256d x, __m256d y);
extern __m256d hs_simdy_maximum_doublex4(__m256d x, __m256d y);
extern __m256d hs_simdy_minimumNumber_doublex4(__m256d x, __m256d y);
extern __m256d hs_simdy_maximumNumber_doublex4(__m256d x, __m256d y);
#endif
#if defined(__AVX512DQ__) && defined(__AVX512VL__)
extern __m128d hs_simdy_minimum_doublex2_avx512(__m128d x, __m128d y);
extern __m128d hs_simdy_maximum_doublex2_avx512(__m128d x, __m128d y);
extern __m128d hs_simdy_minimumNumber_doublex2_avx512(__m128d x, __m128d y);
extern __m128d hs_simdy_maximumNumber_doublex2_avx512(__m128d x, __m128d y);
extern __m256d hs_simdy_minimum_doublex4_avx512(__m256d x, __m256d y);
extern __m256d hs_simdy_maximum_doublex4_avx512(__m256d x, __m256d y);
extern __m256d hs_simdy_minimumNumber_doublex4_avx512(__m256d x, __m256d y);
extern __m256d hs_simdy_maximumNumber_doublex4_avx512(__m256d x, __m256d y);
#endif
#if defined(__AVX512DQ__)
extern __m512d hs_simdy_minimum_doublex8(__m512d x, __m512d y);
extern __m512d hs_simdy_maximum_doublex8(__m512d x, __m512d y);
extern __m512d hs_simdy_minimumNumber_doublex8(__m512d x, __m512d y);
extern __m512d hs_simdy_maximumNumber_doublex8(__m512d x, __m512d y);
#endif
#elif defined(__aarch64__)
float64x2_t hs_simdy_minimum_doublex2(float64x2_t x, float64x2_t y);
float64x2_t hs_simdy_maximum_doublex2(float64x2_t x, float64x2_t y);
float64x2_t hs_simdy_minimumNumber_doublex2(float64x2_t x, float64x2_t y);
float64x2_t hs_simdy_maximumNumber_doublex2(float64x2_t x, float64x2_t y);
#endif

void minimum_double_array_2_portable(const double x[2], const double y[2], double result[restrict 2])
{
    for (int i = 0; i < 2; ++i) {
        result[i] = hs_simdy_minimum_double(x[i], y[i]);
    }
}

void maximum_double_array_2_portable(const double x[2], const double y[2], double result[restrict 2])
{
    for (int i = 0; i < 2; ++i) {
        result[i] = hs_simdy_maximum_double(x[i], y[i]);
    }
}

void minimumNumber_double_array_2_portable(const double x[2], const double y[2], double result[restrict 2])
{
    for (int i = 0; i < 2; ++i) {
        result[i] = hs_simdy_minimumNumber_double(x[i], y[i]);
    }
}

void maximumNumber_double_array_2_portable(const double x[2], const double y[2], double result[restrict 2])
{
    for (int i = 0; i < 2; ++i) {
        result[i] = hs_simdy_maximumNumber_double(x[i], y[i]);
    }
}

#if defined(__SSE2__)
void minimum_double_array_2_sse(const double x[2], const double y[2], double result[restrict 2])
{
    __m128d xx = _mm_loadu_pd(x);
    __m128d yy = _mm_loadu_pd(y);
    __m128d zz = hs_simdy_minimum_doublex2(xx, yy);
    _mm_storeu_pd(result, zz);
}
void maximum_double_array_2_sse(const double x[2], const double y[2], double result[restrict 2])
{
    __m128d xx = _mm_loadu_pd(x);
    __m128d yy = _mm_loadu_pd(y);
    __m128d zz = hs_simdy_maximum_doublex2(xx, yy);
    _mm_storeu_pd(result, zz);
}
void minimumNumber_double_array_2_sse(const double x[2], const double y[2], double result[restrict 2])
{
    __m128d xx = _mm_loadu_pd(x);
    __m128d yy = _mm_loadu_pd(y);
    __m128d zz = hs_simdy_minimumNumber_doublex2(xx, yy);
    _mm_storeu_pd(result, zz);
}
void maximumNumber_double_array_2_sse(const double x[2], const double y[2], double result[restrict 2])
{
    __m128d xx = _mm_loadu_pd(x);
    __m128d yy = _mm_loadu_pd(y);
    __m128d zz = hs_simdy_maximumNumber_doublex2(xx, yy);
    _mm_storeu_pd(result, zz);
}
#endif

#if defined(__AVX__)
void minimum_double_array_4_avx(const double x[4], const double y[4], double result[restrict 4])
{
    __m256d xx = _mm256_loadu_pd(x);
    __m256d yy = _mm256_loadu_pd(y);
    __m256d zz = hs_simdy_minimum_doublex4(xx, yy);
    _mm256_storeu_pd(result, zz);
}
void maximum_double_array_4_avx(const double x[4], const double y[4], double result[restrict 4])
{
    __m256d xx = _mm256_loadu_pd(x);
    __m256d yy = _mm256_loadu_pd(y);
    __m256d zz = hs_simdy_maximum_doublex4(xx, yy);
    _mm256_storeu_pd(result, zz);
}
void minimumNumber_double_array_4_avx(const double x[4], const double y[4], double result[restrict 4])
{
    __m256d xx = _mm256_loadu_pd(x);
    __m256d yy = _mm256_loadu_pd(y);
    __m256d zz = hs_simdy_minimumNumber_doublex4(xx, yy);
    _mm256_storeu_pd(result, zz);
}
void maximumNumber_double_array_4_avx(const double x[4], const double y[4], double result[restrict 4])
{
    __m256d xx = _mm256_loadu_pd(x);
    __m256d yy = _mm256_loadu_pd(y);
    __m256d zz = hs_simdy_maximumNumber_doublex4(xx, yy);
    _mm256_storeu_pd(result, zz);
}
#endif

#if defined(__AVX512DQ__) && defined(__AVX512VL__)
void minimum_double_array_2_avx512(const double x[2], const double y[2], double result[restrict 2])
{
    __m128d xx = _mm_loadu_pd(x);
    __m128d yy = _mm_loadu_pd(y);
    __m128d zz = hs_simdy_minimum_doublex2_avx512(xx, yy);
    _mm_storeu_pd(result, zz);
}
void maximum_double_array_2_avx512(const double x[2], const double y[2], double result[restrict 2])
{
    __m128d xx = _mm_loadu_pd(x);
    __m128d yy = _mm_loadu_pd(y);
    __m128d zz = hs_simdy_maximum_doublex2_avx512(xx, yy);
    _mm_storeu_pd(result, zz);
}
void minimumNumber_double_array_2_avx512(const double x[2], const double y[2], double result[restrict 2])
{
    __m128d xx = _mm_loadu_pd(x);
    __m128d yy = _mm_loadu_pd(y);
    __m128d zz = hs_simdy_minimumNumber_doublex2_avx512(xx, yy);
    _mm_storeu_pd(result, zz);
}
void maximumNumber_double_array_2_avx512(const double x[2], const double y[2], double result[restrict 2])
{
    __m128d xx = _mm_loadu_pd(x);
    __m128d yy = _mm_loadu_pd(y);
    __m128d zz = hs_simdy_maximumNumber_doublex2_avx512(xx, yy);
    _mm_storeu_pd(result, zz);
}
void minimum_double_array_4_avx512(const double x[4], const double y[4], double result[restrict 4])
{
    __m256d xx = _mm256_loadu_pd(x);
    __m256d yy = _mm256_loadu_pd(y);
    __m256d zz = hs_simdy_minimum_doublex4_avx512(xx, yy);
    _mm256_storeu_pd(result, zz);
}
void maximum_double_array_4_avx512(const double x[4], const double y[4], double result[restrict 4])
{
    __m256d xx = _mm256_loadu_pd(x);
    __m256d yy = _mm256_loadu_pd(y);
    __m256d zz = hs_simdy_maximum_doublex4_avx512(xx, yy);
    _mm256_storeu_pd(result, zz);
}
void minimumNumber_double_array_4_avx512(const double x[4], const double y[4], double result[restrict 4])
{
    __m256d xx = _mm256_loadu_pd(x);
    __m256d yy = _mm256_loadu_pd(y);
    __m256d zz = hs_simdy_minimumNumber_doublex4_avx512(xx, yy);
    _mm256_storeu_pd(result, zz);
}
void maximumNumber_double_array_4_avx512(const double x[4], const double y[4], double result[restrict 4])
{
    __m256d xx = _mm256_loadu_pd(x);
    __m256d yy = _mm256_loadu_pd(y);
    __m256d zz = hs_simdy_maximumNumber_doublex4_avx512(xx, yy);
    _mm256_storeu_pd(result, zz);
}
void minimum_double_array_8_avx512(const double x[8], const double y[8], double result[restrict 8])
{
    __m512d xx = _mm512_loadu_pd(x);
    __m512d yy = _mm512_loadu_pd(y);
    __m512d zz = hs_simdy_minimum_doublex8(xx, yy);
    _mm512_storeu_pd(result, zz);
}
void maximum_double_array_8_avx512(const double x[8], const double y[8], double result[restrict 8])
{
    __m512d xx = _mm512_loadu_pd(x);
    __m512d yy = _mm512_loadu_pd(y);
    __m512d zz = hs_simdy_maximum_doublex8(xx, yy);
    _mm512_storeu_pd(result, zz);
}
void minimumNumber_double_array_8_avx512(const double x[8], const double y[8], double result[restrict 8])
{
    __m512d xx = _mm512_loadu_pd(x);
    __m512d yy = _mm512_loadu_pd(y);
    __m512d zz = hs_simdy_minimumNumber_doublex8(xx, yy);
    _mm512_storeu_pd(result, zz);
}
void maximumNumber_double_array_8_avx512(const double x[8], const double y[8], double result[restrict 8])
{
    __m512d xx = _mm512_loadu_pd(x);
    __m512d yy = _mm512_loadu_pd(y);
    __m512d zz = hs_simdy_maximumNumber_doublex8(xx, yy);
    _mm512_storeu_pd(result, zz);
}
#endif

#if defined(__aarch64__)
void minimum_double_array_2_neon(const double x[2], const double y[2], double result[restrict 2])
{
    float64x2_t xx = vld1q_f64(x);
    float64x2_t yy = vld1q_f64(y);
    float64x2_t zz = hs_simdy_minimum_doublex2(xx, yy);
    vst1q_f64(result, zz);
}
void maximum_double_array_2_neon(const double x[2], const double y[2], double result[restrict 2])
{
    float64x2_t xx = vld1q_f64(x);
    float64x2_t yy = vld1q_f64(y);
    float64x2_t zz = hs_simdy_maximum_doublex2(xx, yy);
    vst1q_f64(result, zz);
}
void minimumNumber_double_array_2_neon(const double x[2], const double y[2], double result[restrict 2])
{
    float64x2_t xx = vld1q_f64(x);
    float64x2_t yy = vld1q_f64(y);
    float64x2_t zz = hs_simdy_minimumNumber_doublex2(xx, yy);
    vst1q_f64(result, zz);
}
void maximumNumber_double_array_2_neon(const double x[2], const double y[2], double result[restrict 2])
{
    float64x2_t xx = vld1q_f64(x);
    float64x2_t yy = vld1q_f64(y);
    float64x2_t zz = hs_simdy_maximumNumber_doublex2(xx, yy);
    vst1q_f64(result, zz);
}
#endif

bool my_issignaling(double x)
{
    uint64_t i;
    memcpy(&i, &x, 8);
    return (i & 0x7FF8000000000000) == 0x7FF0000000000000 && (i & 0x7FFFFFFFFFFFFFFF) != 0x7FF0000000000000;
}

bool same_double(double x, double y)
{
    if (isnan(x) && isnan(y)) {
        return my_issignaling(x) == my_issignaling(y);
    } else {
        return x == y && signbit(x) == signbit(y);
    }
}

void print_double(double x)
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
    double x;
    double y;
    double min;
    double max;
    double minnm;
    double maxnm;
    bool invalid;
} test_data[] = {
    {/* SNAN */ 0.0, NAN, NAN, NAN, NAN, NAN, true},
    {NAN, /* SNAN */ 0.0, NAN, NAN, NAN, NAN, true},
    {/* SNAN */ 0.0, 5.0, NAN, NAN, 5.0, 5.0, true},
    {-0.0, /* SNAN */ 0.0, NAN, NAN, -0.0, -0.0, true},
    {/* SNAN */ 0.0, /* SNAN */ 0.0, NAN, NAN, NAN, NAN, true},
    {1.0, 2.0, 1.0, 2.0, 1.0, 2.0, false},
    {5.0, 2.0, 2.0, 5.0, 2.0, 5.0, false},
    {5.0, 0.0, 0.0, 5.0, 0.0, 5.0, false},
    {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, false},
    {0.0, -0.0, -0.0, 0.0, -0.0, 0.0, false},
    {-0.0, 0.0, -0.0, 0.0, -0.0, 0.0, false},
    {-0.0, -0.0, -0.0, -0.0, -0.0, -0.0, false},
    {INFINITY, -INFINITY, -INFINITY, INFINITY, -INFINITY, INFINITY, false},
    {-333.0, -INFINITY, -INFINITY, -333.0, -INFINITY, -333.0, false},
    {-0.0, INFINITY, -0.0, INFINITY, -0.0, INFINITY, false},
    {INFINITY, NAN, NAN, NAN, INFINITY, INFINITY, false},
    {NAN, 0.0, NAN, NAN, 0.0, 0.0, false},
    {3.0, NAN, NAN, NAN, 3.0, 3.0, false},
    {NAN, NAN, NAN, NAN, NAN, NAN, false},
};

void set_snan(double *result)
{
    uint64_t i = 0x7FF0000000000001;
    memcpy(result, &i, 8);
}

void test_commutativity(const char *name, double (*f)(double, double))
{
    for (int i = 0; i < sizeof(test_data) / sizeof(test_data[0]); ++i) {
        double x = test_data[i].x;
        double y = test_data[i].y;
        feclearexcept(FE_ALL_EXCEPT);
        double result1 = f(x, y);
        int excepts1 = fetestexcept(FE_ALL_EXCEPT);
        feclearexcept(FE_ALL_EXCEPT);
        double result2 = f(y, x);
        int excepts2 = fetestexcept(FE_ALL_EXCEPT);
        bool ok = same_double(result1, result2) && excepts1 == excepts2;
        if (!ok) {
            printf("%s(", name);
            print_double(x);
            printf(", ");
            print_double(y);
            printf(") != %s", name);
            print_double(y);
            printf(", ");
            print_double(x);
            printf(")\n");
            exit(1);
        }
    }
    printf("commutativity for %s...OK\n", name);
}

void test_scalar(const char *name, double (*f)(double, double), size_t offset)
{
    for (int i = 0; i < sizeof(test_data) / sizeof(test_data[0]); ++i) {
        double x = test_data[i].x;
        double y = test_data[i].y;
        double expected = *(const double *)((const char *)&test_data[i] + offset);
        bool invalid = test_data[i].invalid;
        feclearexcept(FE_ALL_EXCEPT);
        double result = f(x, y);
        int excepts = fetestexcept(FE_ALL_EXCEPT);
        bool ok = same_double(result, expected) && excepts == (invalid ? FE_INVALID : 0);
        printf("%s(", name);
        print_double(x);
        printf(", ");
        print_double(y);
        printf(") = ");
        print_double(result);
        printf(", with ");
        print_exception(excepts);
        if (ok) {
            printf("...OK\n");
        } else {
            printf("...expected ");
            print_double(expected);
            printf(" with %s\n", invalid ? "INVALID" : "no exception");
            exit(1);
        }
    }
}

void test_array(size_t n, const char *name, void (*f_arr)(const double [], const double [], double [restrict]), size_t offset)
{
    for (int i = 0; i < sizeof(test_data) / sizeof(test_data[0]); ++i) {
        double x = test_data[i].x;
        double y = test_data[i].y;
        double expected = *(const double *)((const char *)&test_data[i] + offset);
        bool invalid = test_data[i].invalid;
        double xarr[n];
        double yarr[n];
        double resultarr[n];
        for (int j = 0; j < n; ++j) {
            xarr[j] = x;
            yarr[j] = y;
        }
        feclearexcept(FE_ALL_EXCEPT);
        f_arr(xarr, yarr, resultarr);
        int excepts = fetestexcept(FE_ALL_EXCEPT);
        bool ok = excepts == (invalid ? FE_INVALID : 0);
        for (int j = 0; j < n; ++j) {
            ok = ok && same_double(resultarr[j], expected);
        }
        printf("%s(", name);
        print_double(x);
        printf(", ");
        print_double(y);
        printf(") = ");
        print_double(resultarr[0]);
        printf(", with ");
        print_exception(excepts);
        if (ok) {
            printf("...OK\n");
        } else {
            printf("...expected ");
            print_double(expected);
            printf(" with %s\n", invalid ? "INVALID" : "no exception");
            exit(1);
        }
    }
    bool ok = true;
    for (int i0 = 0; i0 < sizeof(test_data) / sizeof(test_data[0]); ++i0) {
        double x0 = test_data[i0].x;
        double y0 = test_data[i0].y;
        double expected0 = *(const double *)((const char *)&test_data[i0] + offset);
        bool invalid0 = test_data[i0].invalid;
        for (int i1 = 0; i1 < sizeof(test_data) / sizeof(test_data[0]); ++i1) {
            double x1 = test_data[i1].x;
            double y1 = test_data[i1].y;
            double expected1 = *(const double *)((const char *)&test_data[i1] + offset);
            bool invalid1 = test_data[i1].invalid;
            double xarr[n];
            double yarr[n];
            double resultarr[n];
            for (int j = 0; j < n; ++j) {
                xarr[j] = j % 2 == 0 ? x0 : x1;
                yarr[j] = j % 2 == 0 ? y0 : y1;
            }
            feclearexcept(FE_ALL_EXCEPT);
            f_arr(xarr, yarr, resultarr);
            int excepts = fetestexcept(FE_ALL_EXCEPT);
            ok = ok && excepts == (invalid0 || invalid1 ? FE_INVALID : 0);
            for (int j = 0; j < n; ++j) {
                ok = ok && same_double(resultarr[j], j % 2 == 0 ? expected0 : expected1);
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
    test_scalar("hs_simdy_minimum_double", hs_simdy_minimum_double, offsetof(struct test_data, min));
    test_scalar("hs_simdy_maximum_double", hs_simdy_maximum_double, offsetof(struct test_data, max));
    test_scalar("hs_simdy_minimumNumber_double", hs_simdy_minimumNumber_double, offsetof(struct test_data, minnm));
    test_scalar("hs_simdy_maximumNumber_double", hs_simdy_maximumNumber_double, offsetof(struct test_data, maxnm));
    test_commutativity("hs_simdy_minimum_double", hs_simdy_minimum_double);
    test_commutativity("hs_simdy_maximum_double", hs_simdy_maximum_double);
    test_commutativity("hs_simdy_minimumNumber_double", hs_simdy_minimumNumber_double);
    test_commutativity("hs_simdy_maximumNumber_double", hs_simdy_maximumNumber_double);
    test_array(2, "minimum_double_array_2_portable", minimum_double_array_2_portable, offsetof(struct test_data, min));
    test_array(2, "maximum_double_array_2_portable", maximum_double_array_2_portable, offsetof(struct test_data, max));
    test_array(2, "minimumNumber_double_array_2_portable", minimumNumber_double_array_2_portable, offsetof(struct test_data, minnm));
    test_array(2, "maximumNumber_double_array_2_portable", maximumNumber_double_array_2_portable, offsetof(struct test_data, maxnm));
#if defined(__SSE2__)
    test_array(2, "minimum_double_array_2_sse", minimum_double_array_2_sse, offsetof(struct test_data, min));
    test_array(2, "maximum_double_array_2_sse", maximum_double_array_2_sse, offsetof(struct test_data, max));
    test_array(2, "minimumNumber_double_array_2_sse", minimumNumber_double_array_2_sse, offsetof(struct test_data, minnm));
    test_array(2, "maximumNumber_double_array_2_sse", maximumNumber_double_array_2_sse, offsetof(struct test_data, maxnm));
#endif
#if defined(__AVX__)
    test_array(4, "minimum_double_array_4_avx", minimum_double_array_4_avx, offsetof(struct test_data, min));
    test_array(4, "maximum_double_array_4_avx", maximum_double_array_4_avx, offsetof(struct test_data, max));
    test_array(4, "minimumNumber_double_array_4_avx", minimumNumber_double_array_4_avx, offsetof(struct test_data, minnm));
    test_array(4, "maximumNumber_double_array_4_avx", maximumNumber_double_array_4_avx, offsetof(struct test_data, maxnm));
#endif
#if defined(__AVX512DQ__) && defined(__AVX512VL__)
    test_array(2, "minimum_double_array_2_avx512", minimum_double_array_2_avx512, offsetof(struct test_data, min));
    test_array(2, "maximum_double_array_2_avx512", maximum_double_array_2_avx512, offsetof(struct test_data, max));
    test_array(2, "minimumNumber_double_array_2_avx512", minimumNumber_double_array_2_avx512, offsetof(struct test_data, minnm));
    test_array(2, "maximumNumber_double_array_2_avx512", maximumNumber_double_array_2_avx512, offsetof(struct test_data, maxnm));
    test_array(4, "minimum_double_array_4_avx512", minimum_double_array_4_avx512, offsetof(struct test_data, min));
    test_array(4, "maximum_double_array_4_avx512", maximum_double_array_4_avx512, offsetof(struct test_data, max));
    test_array(4, "minimumNumber_double_array_4_avx512", minimumNumber_double_array_4_avx512, offsetof(struct test_data, minnm));
    test_array(4, "maximumNumber_double_array_4_avx512", maximumNumber_double_array_4_avx512, offsetof(struct test_data, maxnm));
    test_array(8, "minimum_double_array_8_avx512", minimum_double_array_8_avx512, offsetof(struct test_data, min));
    test_array(8, "maximum_double_array_8_avx512", maximum_double_array_8_avx512, offsetof(struct test_data, max));
    test_array(8, "minimumNumber_double_array_8_avx512", minimumNumber_double_array_8_avx512, offsetof(struct test_data, minnm));
    test_array(8, "maximumNumber_double_array_8_avx512", maximumNumber_double_array_8_avx512, offsetof(struct test_data, maxnm));
#endif
#if defined(__aarch64__)
    test_array(2, "minimum_double_array_2_neon", minimum_double_array_2_neon, offsetof(struct test_data, min));
    test_array(2, "maximum_double_array_2_neon", maximum_double_array_2_neon, offsetof(struct test_data, max));
    test_array(2, "minimumNumber_double_array_2_neon", minimumNumber_double_array_2_neon, offsetof(struct test_data, minnm));
    test_array(2, "maximumNumber_double_array_2_neon", maximumNumber_double_array_2_neon, offsetof(struct test_data, maxnm));
#endif
}
