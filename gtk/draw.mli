open Bimage

type t

val draw :
  (Cairo.Surface.t -> unit) -> (int, u8, [ gray | rgb ]) Image.t -> unit
