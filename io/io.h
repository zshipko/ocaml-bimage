#pragma once

#include <memory>

#include <OpenImageIO/imagebuf.h>
#include <OpenImageIO/imageio.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

using namespace OIIO;
using std::unique_ptr;

#define ImageSpec_val(v) (*((ImageSpec **)Data_custom_val(v)))
#define ImageInput_val(v) (*((ImageInput **)Data_custom_val(v)))
#define ImageOutput_val(v) (*((ImageOutput **)Data_custom_val(v)))

value alloc_spec(ImageSpec spec);
