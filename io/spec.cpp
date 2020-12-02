#include "io.h"

static void free_spec(value i) {
  ImageSpec *spec = ImageSpec_val(i);
  delete spec;
}

static struct custom_operations Spec_ops = {
    "zshipko.bimage.Spec",      free_spec,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

value alloc_spec(ImageSpec spec) {
  value v = caml_alloc_custom(&Spec_ops, sizeof(ImageSpec *), 0, 1);
  ImageSpec_val(v) = new ImageSpec(spec);
  return v;
}

extern "C" value image_spec(value width, value height, value channels,
                            value index, value base_type) {
  CAMLparam4(width, height, channels, base_type);
  CAMLlocal1(spec);
  spec =
      alloc_spec(ImageSpec(Int_val(width), Int_val(height), Int_val(channels),
                           (TypeDesc::BASETYPE)Int_val(base_type)));
  CAMLreturn(spec);
}

extern "C" value spec_shape(value s) {
  CAMLparam1(s);
  CAMLlocal1(shape);
  auto spec = ImageSpec_val(s);
  shape = caml_alloc(3, 0);
  Store_field(shape, 0, Val_int(spec->width));
  Store_field(shape, 1, Val_int(spec->height));
  Store_field(shape, 2, Val_int(spec->nchannels));
  CAMLreturn(shape);
}

extern "C" value spec_base_type(value s) {
  CAMLparam1(s);
  CAMLlocal1(bt);
  auto spec = ImageSpec_val(s);
  bt = Val_int((int)spec->format.basetype);
  CAMLreturn(bt);
}
