module Texture : sig
  type t = {
    dirty : bool;
    id : int;
    internal : int;
    kind : int;
    color : int;
    framebuffer : int;
    width : int;
    height : int;
    has_alpha : bool;
  }

  val create : ('a, 'b, [< `Rgb | `Rgba ]) Bimage.Image.t -> t

  val draw :
    t -> GLFW.window -> ('a, 'b, [< `Rgb | `Rgba ]) Bimage.Image.t -> unit
end
