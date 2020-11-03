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

static value alloc_output(ImageOutput *output) {
  value v = caml_alloc_custom(&Output_ops, sizeof(ImageOutput *), 0, 1);
  ImageOutput_val(v) = output;
  return v;
}

value output_create(value filename) {
  CAMLparam1(filename);
  CAMLlocal1(output);
  try {
    auto image_output = ImageOutput::create(String_val(filename));
    output = alloc_output(image_output.release());
  } catch (std::exception exc) {
    caml_failwith(exc.what());
  }
  CAMLreturn(output);
}

value output_set_spec(value output, value filename, value spec) {
  CAMLparam3(output, filename, spec);
  ImageOutput_val(output)->open(String_val(filename), *ImageSpec_val(spec));
  CAMLreturn(Val_unit);
}
