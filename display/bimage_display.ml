open Bimage

module Texture = struct
  type t = {
    has_alpha : bool;
    id : int;
    internal : int;
    kind : int;
    color : int;
    framebuffer : int;
    width : int;
    height : int;
  }

  external create :
    int -> int -> bool -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> t
    = "bimage_create_texture"

  external draw :
    t -> int -> int -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> unit
    = "bimage_draw_texture"

  let create image =
    create image.Image.width image.Image.height
      (Color.has_alpha image.Image.color)
      image.Image.data

  let draw t window image =
    let w, h = GLFW.getWindowSize ~window in
    if image.Image.width = t.width && image.Image.height = t.height then
      let () = draw t w h image.Image.data in
      GLFW.swapBuffers ~window
end
