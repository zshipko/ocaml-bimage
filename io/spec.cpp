#include "io.hpp"

static struct custom_operations Output_ops = {
    "zshipko.bimage.Spec",      custom_finalize_default,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

static value alloc_spec(ImageSpec spec) {
  value v = caml_alloc_custom(&Output_ops, sizeof(ImageSpec), 0, 1);
  *ImageSpec_val(v) = spec;
  return v;
}

value image_spec(value width, value height, value channels, value base_type) {
  return alloc_spec(ImageSpec(Int_val(width), Int_val(height),
                              Int_val(channels),
                              (TypeDesc::BASETYPE)Int_val(base_type)));
}
