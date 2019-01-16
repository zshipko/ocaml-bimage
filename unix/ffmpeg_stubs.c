#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <unistd.h>

value write_u8_bigarray(value file_descr, value bigarray) {
  CAMLparam2(file_descr, bigarray);
  int len = Caml_ba_array_val(bigarray)->dim[0];
  if (write(Int_val(file_descr), Caml_ba_data_val(bigarray), len) < 0) {
    caml_failwith("Error writing to ffmpeg output");
  }
  CAMLreturn(Val_unit);
}
