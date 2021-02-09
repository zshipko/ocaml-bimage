#include <stdbool.h>

#include <GL/glew.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value bimage_create_texture(value width, value height, value channels,
                            value data) {
  glewExperimental = GL_TRUE;
  glewInit();
  CAMLparam4(width, height, channels, data);
  CAMLlocal1(tex);

  if (Int_val(channels) != 3 && Int_val(channels) != 4) {
    caml_failwith("Invalid image type");
  }

  GLuint framebuffer = 0, texture_id = 0, texture_internal = 0,
         texture_kind = 0, texture_color = 0;

  // Setup texture
  glGenTextures(1, &texture_id);
  glBindTexture(GL_TEXTURE_2D, texture_id);

  texture_color = Int_val(channels) == 3 ? GL_RGB : GL_RGBA;

  switch (Caml_ba_array_val(data)->flags & BIGARRAY_KIND_MASK) {
  case CAML_BA_UINT8:
    texture_kind = GL_UNSIGNED_BYTE;
    break;
  case CAML_BA_UINT16:
    texture_kind = GL_UNSIGNED_SHORT;
    break;
  case CAML_BA_FLOAT32:
    texture_kind = GL_FLOAT;
    break;
  case CAML_BA_INT32:
    texture_kind = GL_INT;
    break;
  default:
    caml_failwith("Invalid image type");
    break;
  }

  if (texture_color == GL_RGB) {
    if (texture_kind == GL_UNSIGNED_BYTE) {
      texture_internal = GL_RGB8;
    } else if (texture_kind == GL_UNSIGNED_SHORT) {
      texture_internal = GL_RGB16;
    } else if (texture_kind == GL_FLOAT) {
      texture_internal = GL_RGB32F;
    } else if (texture_kind == GL_INT) {
      texture_internal = GL_RGB32I;
    }
  } else if (texture_color == GL_RGBA) {
    if (texture_kind == GL_UNSIGNED_BYTE) {
      texture_internal = GL_RGBA8;
    } else if (texture_kind == GL_UNSIGNED_SHORT) {
      texture_internal = GL_RGBA16;
    } else if (texture_kind == GL_FLOAT) {
      texture_internal = GL_RGBA32F;
    } else if (texture_kind == GL_INT) {
      texture_internal = GL_RGBA32I;
    }
  } else {
    caml_failwith("Invalid image type");
  }

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexImage2D(GL_TEXTURE_2D, 0, texture_internal, Int_val(width),
               Int_val(height), 0, texture_color, texture_kind,
               Caml_ba_data_val(data));
  glBindTexture(GL_TEXTURE_2D, 0);

  // Setup framebuffer
  glGenFramebuffers(1, &framebuffer);
  glBindFramebuffer(GL_READ_FRAMEBUFFER, framebuffer);
  glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                         GL_TEXTURE_2D, texture_id, 0);
  glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);

  tex = caml_alloc(8, 0);
  Store_field(tex, 0, channels);
  Store_field(tex, 1, Val_int(texture_id));
  Store_field(tex, 2, Val_int(texture_internal));
  Store_field(tex, 3, Val_int(texture_kind));
  Store_field(tex, 4, Val_int(texture_color));
  Store_field(tex, 5, Val_int(framebuffer));
  Store_field(tex, 6, width);
  Store_field(tex, 7, height);

  CAMLreturn(tex);
}

value bimage_draw_texture(value tex, value window_width, value window_height,
                          value data) {
  CAMLparam4(tex, window_width, window_height, data);

  GLuint texture_id = (GLuint)Int_val(Field(tex, 1));
  GLuint texture_internal = (GLuint)Int_val(Field(tex, 2));
  GLuint texture_kind = (GLuint)Int_val(Field(tex, 3));
  GLuint texture_color = (GLuint)Int_val(Field(tex, 4));
  GLuint framebuffer = (GLuint)Int_val(Field(tex, 5));
  int width = Int_val(Field(tex, 6));
  int height = Int_val(Field(tex, 7));
  int win_width = Int_val(window_width);
  int win_height = Int_val(window_height);

  double w_ratio = (double)win_width / (double)width;
  double h_ratio = (double)win_height / (double)height;
  double ratio = w_ratio < h_ratio ? w_ratio : h_ratio;
  int display_width = (int)(width * ratio);
  int display_height = (int)(height * ratio);
  int x = (win_width - display_width) / 2;
  int y = (win_height - display_height) / 2;

  if (x < 0)
    x = 0;
  if (y < 0)
    y = 0;

  glViewport(0, 0, win_width, win_height);
  glClearColor(0.0, 0.0, 0.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);

  glBindTexture(GL_TEXTURE_2D, texture_id);
  glTexImage2D(GL_TEXTURE_2D, 0, texture_internal, width, height, 0,
               texture_color, texture_kind, Caml_ba_data_val(data));
  glBindTexture(GL_TEXTURE_2D, 0);

  glBindFramebuffer(GL_READ_FRAMEBUFFER, framebuffer);
  glViewport(0, 0, 512, 512);
  glFramebufferTexture2D(GL_READ_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
                         GL_TEXTURE_2D, texture_id, 0);
  glBlitFramebuffer(0, height, width, 0, x, y, x + display_width,
                    y + display_height, GL_COLOR_BUFFER_BIT, GL_NEAREST);
  glBindFramebuffer(GL_READ_FRAMEBUFFER, 0);

  CAMLreturn(Val_unit);
}
