val pi : float
val e : float

(** The Angle type is used instead of a float whenever a function expects an angle
      argument to avoid ambiguity *)
module Angle : sig
  type t

  val of_degrees : float -> t
  (** [of_degrees deg] creates new angle from [deg] degrees *)

  val to_degrees : t -> float
  (** [to_degrees angle] returns the value of the angle in degrees *)

  val of_radians : float -> t
  (** [of_radians rad] creates a new angle from [rad] radians *)

  val to_radians : t -> float
  (** [to_radians angle] returns the value of the angle in radians *)
end

(** Point is a 2 element float tuple used to perform calculations on (x, y) coordinates *)
module Point : sig
  type t = float * float

  val x : t -> float
  (** [x pt] extracts the x coordinate *)

  val y : t -> float
  (** [y pt] extracts the y coordinate *)
end
