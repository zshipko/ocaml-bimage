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
  val close: t -> unit
  val update: t -> unit

  val mouse_position : t -> float -> float -> (float * float)
  val on_mouse_button: (t -> int -> bool -> GLFW.key_mod list -> unit) -> t -> unit
  val on_mouse_move : (t -> float -> float -> unit) -> t -> unit
  val on_key : (t ->
                GLFW.key -> int -> GLFW.key_action -> GLFW.key_mod list -> unit) -> t -> unit
  val get_mouse_position : t -> float * float
  val get_key : t -> GLFW.key -> bool
  val get_mouse_button : t -> int -> bool
end

val show_all: Window.t list -> unit
val show: (string * Bimage.Image.any) list -> unit
