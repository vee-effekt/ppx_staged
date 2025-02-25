#ifndef COMMON_H_   /* Include guard */
#define COMMON_H_

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

int64_t mix_bits(int64_t z, int64_t n);

int64_t mix64(int64_t z);

bool remainder_is_unbiased(int64_t draw, int64_t remainder, int64_t draw_max, int64_t remainder_max);

double unit_float_from_int64(int64_t n);

#endif