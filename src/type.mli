open Bigarray

module type TYPE = sig
  type t
  (** Bigarray OCaml type *)

  type elt
  (** Bigarray storage type *)

  type kind = (t, elt) Bigarray.kind
  (** Bigarray kind type *)

  val name : string
  (** Type name *)

  val kind : kind
  (** kind value **)

  val of_float : float -> t
  (** Convert from float *)

  val to_float : t -> float
  (** Convert to float *)
end

type ('a, 'b) t = (module TYPE with type t = 'a and type elt = 'b)

val of_kind : ('a, 'b) Bigarray.kind -> ('a, 'b) t

val kind : ('a, 'b) t -> ('a, 'b) Bigarray.kind
(** Get Bigarray kind *)

val name : ('a, 'b) t -> string
(** [name k] returns the name of a given ty *)

val depth : ('a, 'b) t -> int
(** returns the number of bits for a given ty *)

val max : ('a, 'b) t -> 'a
(** [max k] returns the maximum normalized value for [k] *)

val min : ('a, 'b) t -> 'a
(** [min k] returns the minimum normalized value for [k] *)

val max_f : ('a, 'b) t -> float
(** [max k] returns the maximum normalized value for [k] as a float *)

val min_f : ('a, 'b) t -> float
(** [min k] returns the minimum normalized value for [k] as a float *)

val to_float : ('a, 'b) t -> 'a -> float
(** [to_float k x] converts a value of type [k] to float *)

val of_float : ('a, 'b) t -> float -> 'a
(** [of_float k x] converts a float to a value of ty [k] *)

val clamp : ('a, 'b) t -> float -> float
(** Converts a float value to a value within the proper range for the given type *)

val normalize : ('a, 'b) t -> float -> float
(** Scales a value to the range 0.0-1.0 *)

val denormalize : ('a, 'b) t -> float -> float
(** Sclaes a value to the range (type_min-type_max) *)

val convert : from:('a, 'b) t -> ('c, 'd) t -> 'a -> 'c
(** Convert a value of one type to another *)

type u8 = int8_unsigned_elt
type u16 = int16_unsigned_elt
type i32 = int32_elt
type i64 = int64_elt
type f32 = float32_elt
type f64 = float64_elt

val u8 : (int, u8) t
val u16 : (int, u16) t
val i32 : (int32, i32) t
val i64 : (int64, i64) t
val f32 : (float, f32) t
val f64 : (float, f64) t
