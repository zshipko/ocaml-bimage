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
  type 'a t

  val create :
    ?callback:('a t -> unit) ->
    ?width:int ->
    ?height:int ->
    string ->
    ('a, 'b, 'c) Bimage.Image.t ->
    'a ->
    'a t

  val show : 'a t -> unit

  val hide : 'a t -> unit

  val close : 'a t -> unit

  val update : 'a t -> unit

  val data : 'a t -> 'a

  val set_data : 'a t -> 'a -> unit

  val set_callback : 'a t -> ('a t -> unit) option -> unit

  val replace_image : 'x t -> ('a, 'b, 'c) Bimage.Image.t -> unit

  val mouse_position : 'a t -> float -> float -> float * float

  val on_mouse_button :
    ('a t -> int -> bool -> GLFW.key_mod list -> unit) -> 'a t -> unit

  val on_mouse_move : ('a t -> float -> float -> unit) -> 'a t -> unit

  val on_key :
    ('a t -> GLFW.key -> int -> GLFW.key_action -> GLFW.key_mod list -> unit) ->
    'a t ->
    unit

  val get_mouse_position : 'a t -> float * float

  val get_key : 'a t -> GLFW.key -> bool

  val get_mouse_button : 'a t -> int -> bool
end

val show_all : ?update_in_background:bool -> 'a Window.t list -> unit

val show :
  ?update_in_background:bool -> (string * Bimage.Image.any) list -> unit
