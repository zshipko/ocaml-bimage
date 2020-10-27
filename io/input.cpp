#include "io.hpp"

static void free_input(value i) {
  ImageInput *input = ImageInput_val(i);
  input->close();
}

static struct custom_operations Input_ops = {
    "zshipko.bimage.Input",     free_input,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static value alloc_input(ImageInput *input) {
  value v = caml_alloc_custom(&Input_ops, sizeof(ImageInput *), 0, 1);
  ImageInput_val(v) = input;
  return v;
}

value input_open(value filename) {
  CAMLparam1(filename);
  CAMLlocal1(input);
  try {
    auto image_input = ImageInput::open(String_val(filename));
    input = alloc_input(image_input.release());
  } catch (std::exception exc) {
    caml_failwith(exc.what());
  };
  CAMLreturn(input);
}
