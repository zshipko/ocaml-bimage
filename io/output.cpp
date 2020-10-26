#include "io.hpp"

static void free_output(value x) {
  ImageOutput *output = ImageOutput_val(x);
  output->close();
}

static struct custom_operations Output_ops = {
    "zshipko.bimage.Output",    free_output,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static value alloc_output(unique_ptr<ImageOutput> output) {
  value v = caml_alloc_custom(&Output_ops, sizeof(ImageOutput *), 0, 1);
  ImageOutput_val(v) = output.release();
  return v;
}
