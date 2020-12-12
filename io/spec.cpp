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
                            value base_type) {
  CAMLparam4(width, height, channels, base_type);
  CAMLlocal1(spec);
  spec =
      alloc_spec(ImageSpec(Int_val(width), Int_val(height), Int_val(channels),
                           TypeDesc((TypeDesc::BASETYPE)Int_val(base_type))));
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

extern "C" value spec_get_attr(value spec, value key, value ty) {
  CAMLparam3(spec, key, ty);
  CAMLlocal2(v, tmp);
  int t = Int_val(ty);
  ParamValue *p = ImageSpec_val(spec)->find_attribute(
      String_val(key), ty == 0 ? TypeInt : (ty == 1 ? TypeFloat : TypeString));
  if (!p) {
    CAMLreturn(Val_unit);
  }

  v = caml_alloc(1, 0);

  if (p->type() == TypeString) {
    const char *s = *(const char **)p->data();
    tmp = caml_alloc_initialized_string(strlen(s), s);
    Store_field(v, 0, tmp);
  } else if (p->type() == TypeFloat) {
    tmp = caml_copy_double(*(const float *)p->data());
    Store_field(v, 0, tmp);
  } else if (p->type() == TypeInt) {
    tmp = Int_val(*(const int *)p->data());
    Store_field(v, 0, tmp);
  } else if (p->type() == TypeDesc::UINT) {
    tmp = Int_val((int)*(const unsigned int *)p->data());
    Store_field(v, 0, tmp);
  }

  CAMLreturn(v);
}

extern "C" value spec_set_attr(value spec, value key, value ty, value v) {
  CAMLparam4(spec, key, ty, v);
  int kind = Int_val(ty);
  if (kind == 0) {
    ImageSpec_val(spec)->attribute(String_val(key), Int_val(v));
  } else if (kind == 1) {
    ImageSpec_val(spec)->attribute(String_val(key), (float)Double_val(v));
  } else if (kind == 2) {
    ImageSpec_val(spec)->attribute(String_val(key), String_val(v));
  }
  CAMLreturn(Val_unit);
}

extern "C" value spec_get_attr_names(value spec) {
  CAMLparam1(spec);
  CAMLlocal2(a, tmp);
  size_t length = ImageSpec_val(spec)->extra_attribs.size();
  for (size_t i = 0; i < length; ++i) {
    const ParamValue &p = ImageSpec_val(spec)->extra_attribs[i];
    if (p.type() == TypeMatrix) {
      length -= 1;
    }
  }
  a = caml_alloc(length, 0);
  for (size_t i = 0; i < length; ++i) {
    const ParamValue &p = ImageSpec_val(spec)->extra_attribs[i];
    if (p.type() == TypeMatrix) {
      continue;
    }
    tmp = caml_alloc_initialized_string(p.name().size(), p.name().c_str());
    Store_field(a, i, tmp);
  }
  CAMLreturn(a);
}
