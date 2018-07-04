open Bimage

type t

val create:
  ?width:int -> ?height:int ->
  ?mousedown:(int -> int -> unit) -> ?mouseup:(int -> int -> unit) ->
  ?keydown:(int -> unit) -> ?keyup:(int -> unit) ->
  ?timer:(int * (unit -> bool)) ->
  string ->
  (int, u8, rgb) Image.t -> t

val exists: string -> bool
val get: string -> t option
val update: t -> (int, u8, rgb) Image.t -> unit
val resize: width:int -> height:int -> t -> unit
val destroy: t -> unit
val show_all: ?ms:int -> unit -> unit
