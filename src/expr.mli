open Color

type pixel = rgb Pixel.t
type image = Image.any

type 'a t =
  | Kernel : Input.index * Kernel.t -> pixel t
  | Transform : Input.index * Transform.t -> pixel t
  | Image : Input.index -> image t
  | Input : Input.index * int t * int t -> pixel t
  | X : int t
  | Y : int t
  | Int : int -> int t
  | Float : float -> float t
  | Bool : bool -> bool t
  | Gt : 'a t * 'a t -> bool t
  | Eq : 'a t * 'a t -> bool t
  | Lt : 'a t * 'a t -> bool t
  | And : bool t * bool t -> bool t
  | Or : bool t * bool t -> bool t
  | Not : bool t -> bool t
  | Cond : bool t * 'a t * 'a t -> 'a t
  | Func : 'b t * (int -> int -> 'b -> 'a t) -> 'a t
  | Pixel : 'b Pixel.t -> pixel t
  | Pixel_get : pixel t * int t -> float t
  | Pixel_set : pixel t * int t * float t -> pixel t
  | Value : 'a -> 'a t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Shape : Input.index -> (int * int * int) t
  | Option : 'a t option -> 'a option t

val prepare: int ref -> int ref -> 'a t -> Input.t -> 'a

val compute_at :
  ?x:int ref -> ?y:int ref -> pixel t -> Input.t -> int -> int -> pixel
(** Compute value of expression at the given point *)

val int : int -> int t
(** Create an int [Expr] *)

val float : float -> float t
(** Create a float [Expr] *)

val int_of_float : float t -> int t
val float_of_int : int t -> float t
val x : int t
val y : int t
val some : 'a t -> 'a option t
val none : 'a option t
val pixel : 'a Pixel.t -> pixel t
val pixel_map : (float -> float) -> pixel t -> pixel t
val image : ?input:Input.index -> unit -> image t

val get : pixel t -> int t -> float t
(** Get pixel index *)

val set : pixel t -> int t -> float t -> pixel t
(** Set pixel index *)

val set_rgb : pixel t -> float t -> float t -> float t -> pixel t
(** Update pixel with RGB values *)

val input : ?index:Input.index -> int t -> int t -> pixel t
(** Get input data from the specified index *)

val value : 'a -> 'a t
(** Create a Value expr *)

val shape : ?input:Input.index -> unit -> (int * int * int) t

val func : 'b t -> (int -> int -> 'b -> 'a t) -> 'a t
(** Create a Func expr *)

val kernel : ?input:Input.index -> Kernel.t -> pixel t
(** Create a kernel expr from an existing kernel *)

val combine_kernel :
  ?input:Input.index ->
  (float -> float -> float) ->
  Kernel.t t ->
  Kernel.t t ->
  pixel t
(** Create a kernel expession using two kernels combined using the designated operation *)

val kernel_3x3 : ?input:Input.index -> Kernel.t -> pixel t
val gaussian_blur : ?std:float -> ?input:Input.index -> int -> pixel t
val sobel_x : ?input:Input.index -> unit -> pixel t
val sobel_y : ?input:Input.index -> unit -> pixel t
val sobel : ?input:Input.index -> unit -> pixel t

val transform : ?input:Input.index -> Transform.t -> pixel t
(** Apply a transformation *)

val rotate :
  ?input:Input.index -> ?center:Util.Point.t -> Util.Angle.t -> pixel t

val rotate_90 : ?input:Input.index -> unit -> pixel t
val rotate_180 : ?input:Input.index -> unit -> pixel t
val rotate_270 : ?input:Input.index -> unit -> pixel t
val scale : ?input:Input.index -> float -> float -> pixel t
val resize : ?input:Input.index -> int -> int -> pixel t

val pair : 'a t -> 'b t -> ('a * 'b) t
(** Create a new Pair expr, used for joining existing expressions *)

val channels : ?input:Input.index -> unit -> int t
val map : ('b -> 'a t) -> 'b t -> 'a t
val ( let* ) : 'b t -> ('b -> 'a t) -> 'a t
val ( let+ ) : 'b t -> ('b -> 'a) -> 'a t
val map2 : ('a -> 'b -> 'c t) -> 'a t -> 'b t -> 'c t
val fadd : float t -> float t -> float t
val fsub : float t -> float t -> float t
val fmul : float t -> float t -> float t
val fdiv : float t -> float t -> float t
val iadd : int t -> int t -> int t
val isub : int t -> int t -> int t
val imul : int t -> int t -> int t
val idiv : int t -> int t -> int t
val pow : float t -> float t -> float t
val sqrt : float t -> float t
val sin : float t -> float t
val cos : float t -> float t
val tan : float t -> float t
val pi : unit -> float t
val and_ : bool t -> bool t -> bool t
val or_ : bool t -> bool t -> bool t
val not_ : bool t -> bool t
val cond : bool t -> 'a t -> 'a t -> 'a t

val blend : ?input0:Input.index -> ?input1:Input.index -> unit -> pixel t
(** An expression to average two images *)

val min : ?input0:Input.index -> ?input1:Input.index -> unit -> pixel t
(** An expression to take the lowest value from two images *)

val max : ?input0:Input.index -> ?input1:Input.index -> unit -> pixel t
(** An expression to take the highest value from two images *)

val brightness : ?input:Input.index -> float t -> pixel t
(** Multiply each pixel component *)

val grayscale : ?input:Input.index -> unit -> pixel t
val color : ?input:Input.index -> unit -> pixel t
val invert : ?input:Input.index -> unit -> pixel t
val gamma : ?input:Input.index -> float -> pixel t
val gamma_log : ?input:Input.index -> ?gamma:float -> unit -> pixel t
val gamma_lin : ?input:Input.index -> ?gamma:float -> unit -> pixel t

module Infix : sig
  val ( && ) : bool t -> bool t -> bool t
  val ( || ) : bool t -> bool t -> bool t

  val ( + ) : int t -> int t -> int t
  (** Integer addition *)

  val ( - ) : int t -> int t -> int t
  (** Integer subtraction *)

  val ( * ) : int t -> int t -> int t
  (** Integer multiplacation *)

  val ( / ) : int t -> int t -> int t
  (** Integer division *)

  val ( +. ) : float t -> float t -> float t
  (** Float addition *)

  val ( -. ) : float t -> float t -> float t
  (** Float subtraction *)

  val ( *. ) : float t -> float t -> float t
  (** Float multiplication *)

  val ( /. ) : float t -> float t -> float t
  (** Float division *)

  val ( ** ) : float t -> float t -> float t
  (** Pow *)

  val ( ?> ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Operator version of [map] *)

  module Pixel : sig
    val ( + ) : pixel t -> pixel t -> pixel t
    val ( - ) : pixel t -> pixel t -> pixel t
    val ( * ) : pixel t -> pixel t -> pixel t
    val ( / ) : pixel t -> pixel t -> pixel t
    val ( +@ ) : pixel t -> float t -> pixel t
    val ( -@ ) : pixel t -> float t -> pixel t
    val ( *@ ) : pixel t -> float t -> pixel t
    val ( /@ ) : pixel t -> float t -> pixel t
  end

  module Kernel : sig
    val ( + ) : Kernel.t t -> Kernel.t t -> pixel t
    val ( - ) : Kernel.t t -> Kernel.t t -> pixel t
    val ( * ) : Kernel.t t -> Kernel.t t -> pixel t
    val ( / ) : Kernel.t t -> Kernel.t t -> pixel t
  end

  module Transform : sig
    val ( + ) : Transform.t t -> Transform.t t -> pixel t
    val ( - ) : Transform.t t -> Transform.t t -> pixel t
    val ( * ) : Transform.t t -> Transform.t t -> pixel t
    val ( / ) : Transform.t t -> Transform.t t -> pixel t
  end
end
