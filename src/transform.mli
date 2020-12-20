open Util

type t

val v :
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  float ->
  t
(** Create new transformation *)

val neg : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val mmul : t -> t -> t

val smul : t -> float -> t

val transpose : t -> t

val det : t -> float

val inv : t -> t

val translate : float -> float -> t

val rotate : ?center:Point.t -> Angle.t -> t

val scale : float -> float -> t

val transform : t -> Point.t -> Point.t

module Infix : sig
  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t

  val ( * ) : t -> t -> t

  val ( / ) : t -> t -> t
end
