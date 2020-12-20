type t =
  [ `Invalid_shape
  | `Invalid_kernel_shape of int * int
  | `Invalid_input of int
  | `Invalid_color
  | `Msg of string ]

exception Exc of t

val exc : t -> 'a
(** Raises an [Exc] with the provided [Error.t] *)

val to_string : t -> string
(** Returns a string representation of an [Error.t] *)

val unwrap : ('a, t) result -> 'a
(** A convenience function that returns the [Ok] value of a result if possible, otherwise
      it raises the [Error] value *)

val string_of_exn : exn -> string
