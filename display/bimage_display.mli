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

  val create : GLFW.window -> ('a, 'b, [< `Rgb | `Rgba ]) Bimage.Image.t -> t

  val draw :
    t -> GLFW.window -> ('a, 'b, [< `Rgb | `Rgba ]) Bimage.Image.t -> unit
end

module Window : sig
  type t

  val create: ?width:int -> ?height:int -> string -> ('a, 'b, 'c) Bimage.Image.t -> t
  val show: t -> unit
  val hide: t -> unit
  val update: t -> unit
end

val show: (string * Bimage.Image.any) list -> unit
