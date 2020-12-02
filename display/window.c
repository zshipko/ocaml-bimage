#include <GL/gl.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

value caml_clear_color_buffer_bit(void) {
  glClear(GL_COLOR_BUFFER_BIT);
  return Val_unit;
}
