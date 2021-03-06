#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

int call_caml_dummy_fun(int x) {
  static value * f_clsr = NULL;
  if (f_clsr == NULL) {
    f_clsr = caml_named_value("dummy_fun");
  }
  return Int_val(callback(*f_clsr, Val_int(x)));
}

value init_f(int n) {
  return Val_int(n);
}

void call_caml_jit_entry(int **x) {
  static value * jit_entry_closure = NULL;
  value ml_args[6];
  if (jit_entry_closure == NULL) {
    jit_entry_closure = caml_named_value("jit_entry");
  }
  ml_args[0] = caml_alloc_array(init_f, x[0]);
  ml_args[1] = caml_alloc_array(init_f, x[1]);
  ml_args[2] = Val_int(x[2][0]);
  ml_args[3] = Val_int(x[2][1]);
  ml_args[4] = Val_int(x[0]);
  ml_args[5] = Val_int(x[1]);
  caml_callbackN(*jit_entry_closure, 6, ml_args);
  return;
}
