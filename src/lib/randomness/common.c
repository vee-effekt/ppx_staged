#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
// This bug took me forever to figure out. A "logical shift right" requires
// casting to uint64_t first.
int64_t mix_bits(int64_t z, int64_t n) {
  return z ^ ((uint64_t) z >> n);
}

int64_t mix64(int64_t z) {
  z = mix_bits(z,33) * 0xff51afd7ed558ccdL;
  z = mix_bits(z,33) * 0xc4ceb9fe1a85ec53L;
  return mix_bits(z, 33);
}

bool remainder_is_unbiased(int64_t draw, int64_t remainder, int64_t draw_max, int64_t remainder_max){
  return (draw - remainder <= draw_max - remainder_max);
}

double unit_float_from_int64(int64_t n){
  return ((uint64_t) n >> 11) * pow(2,-53);
}