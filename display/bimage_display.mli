module Texture : sig
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

  val create : ('a, 'b, [< `Rgb | `Rgba ]) Bimage.Image.t -> t

  val draw :
    t -> GLFW.window -> ('a, 'b, [< `Rgb | `Rgba ]) Bimage.Image.t -> unit
end
