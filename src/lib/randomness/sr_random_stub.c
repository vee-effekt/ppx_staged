#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include "common.h"


typedef struct state {
  int64_t *seed;
  int64_t odd_gamma;
} state_t;

void fill_from_value(value v, state_t* st){
    int64_t *seed = (int64_t*) Data_custom_val(Field(v,0));
    int64_t odd_gamma = Int64_val(Field(v,1));
    st->seed = seed;
    st->odd_gamma = odd_gamma;
}

int64_t next_seed_sr(state_t *s){
  int64_t next = *(s->seed) + s->odd_gamma;
  *(s->seed) = next;
  return next;
}



int64_t next_int64_sr(state_t* st){
  return mix64(next_seed_sr(st));
}

bool next_bool_sr(state_t *st){
  int64_t draw = next_int64_sr(st);
  return ((draw | 1L) == draw);
}

int64_t between_sr(state_t* st,int64_t lo,int64_t hi){
  int64_t draw = next_int64_sr(st);
  while (lo > draw || hi < draw){
    draw = next_int64_sr(st);
  }
  return draw;
}

int64_t non_negative_up_to_sr(state_t *st, int64_t max){
  int64_t draw;
  int64_t remainder;
  do {
    draw = next_int64_sr(st) & INT64_MAX;
    remainder = draw % (max + 1);
  } while (!remainder_is_unbiased(draw,remainder,INT64_MAX,max));
  return remainder;
}

int64_t next_int_sr(state_t *st,int64_t lo, int64_t hi){
  int64_t diff = hi - lo;
  int64_t result;
  if(diff == INT64_MAX){
    result = ((next_int64_sr(st) & INT64_MAX) + lo);
  } else if (diff >= 0){
    result = non_negative_up_to_sr(st,diff) + lo;
  } else {
    result = between_sr(st,lo,hi);
  }

  return result;
}

double unit_float_sr(state_t *st){
  return unit_float_from_int64(next_int64_sr(st));
}


double next_float_sr(state_t *st, double lo, double hi){
  while(!(isfinite(hi - lo))){
    double mid = (hi + lo) / 2.0;
    if(next_bool_sr(st)){
      hi = mid;
    } else {
      lo = mid;
    }
  }
  return (lo + unit_float_sr(st) * (hi - lo));

}

CAMLprim value bool_c_sr(value sr_state_val) {
  CAMLparam1(sr_state_val);
  state_t* st = (state_t*) alloca(sizeof(state_t));
  fill_from_value(sr_state_val,st);
  CAMLreturn(Val_bool(next_bool_sr(st)));
}

CAMLprim value float_c_sr_unchecked(value sr_state_val, value lo_val, value hi_val){
  CAMLparam3(sr_state_val,lo_val,hi_val);
  state_t* st = (state_t*) alloca(sizeof(state_t));
  fill_from_value(sr_state_val,st);
  double lo = Double_val(lo_val);
  double hi = Double_val(hi_val);
  double result = next_float_sr(st,lo,hi);
  CAMLreturn(caml_copy_double(result));
}

CAMLprim value int_c_sr_unchecked(value sr_state_val, value lo_val, value hi_val) {
  CAMLparam3(sr_state_val,lo_val,hi_val);
  state_t* st = (state_t*) alloca(sizeof(state_t));
  fill_from_value(sr_state_val,st);

  int64_t lo = (int64_t) Int_val(lo_val);
  int64_t hi = (int64_t) Int_val(hi_val);

  int64_t result = next_int_sr(st,lo,hi);
  CAMLreturn(Val_int(result));
}

// CAMLprim value print(value state_val) {
//   CAMLparam1(state_val);
//   state_t st = State_val(state_val);
//   printf("C-Seed: %ld\n", st.seed);
//   CAMLreturn(Val_unit);
// }