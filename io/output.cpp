#include "io.h"

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

extern "C" value output_create(value filename) {
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

extern "C" value output_open(value output, value filename, value spec,
                             value is_append) {
  CAMLparam4(output, filename, spec, is_append);
  try {
    auto append_mode = ImageOutput::Create;
    if (Int_val(is_append) != 0) {
      append_mode = ImageOutput::AppendSubimage;
    }
    ImageOutput_val(output)->open(String_val(filename), *ImageSpec_val(spec),
                                  append_mode);
  } catch (std::exception exc) {
    caml_failwith(exc.what());
  }
  CAMLreturn(Val_unit);
}

extern "C" value output_write_image(value output, value spec, value ba) {
  CAMLparam3(output, spec, ba);
  auto out = ImageOutput_val(output);
  void *data = Caml_ba_data_val(ba);
  auto format = ImageSpec_val(spec)->format;
  caml_release_runtime_system();
  try {
    out->write_image(format, data);
    caml_acquire_runtime_system();
  } catch (std::exception exc) {
    caml_acquire_runtime_system();
    caml_failwith(exc.what());
  }

  CAMLreturn(Val_unit);
}
