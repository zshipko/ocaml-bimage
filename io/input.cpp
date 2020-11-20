#include "io.h"

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

extern "C" value input_open(value filename) {
  CAMLparam1(filename);
  CAMLlocal1(input);
  try {
    auto image_input = ImageInput::open(String_val(filename));
    if (!image_input) {
      caml_failwith("Invalid image");
    }
    input = alloc_input(image_input.release());
  } catch (std::exception exc) {
    caml_failwith(exc.what());
  };
  CAMLreturn(input);
}

extern "C" value input_get_spec(value input) {
  CAMLparam1(input);
  CAMLlocal1(spec);
  ImageSpec s = ImageInput_val(input)->spec();
  spec = alloc_spec(s);
  CAMLreturn(spec);
}

extern "C" value input_read(value input, value channels, value index,
                            value spec, value ba) {
  CAMLparam5(input, channels, index, spec, ba);
  ImageSpec *s = ImageSpec_val(spec);
  auto i = ImageInput_val(input);
  void *data = Caml_ba_data_val(ba);
  caml_release_runtime_system();
  try {
    i->read_image(Int_val(index), 0, 0, Int_val(channels), s->format, data);
    caml_acquire_runtime_system();
  } catch (std::exception exc) {
    caml_acquire_runtime_system();
    caml_failwith(exc.what());
  }
  CAMLreturn(Val_unit);
}
