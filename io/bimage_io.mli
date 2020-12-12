type error = [ Bimage.Error.t | `File_not_found of string ]

type base_type =
  | Unknown
  | None
  | UInt8
  | Int8
  | UInt16
  | Int16
  | UInt32
  | Int32
  | UInt64
  | Int64
  | Half
  | Float
  | Double
  | String
  | Ptr

module Spec : sig
  type t

  val shape : t -> int * int * int

  val base_type : t -> base_type
end

module Input : sig
  type t

  val init : string -> (t, error) result

  val spec : t -> Spec.t

  val read_image :
    ?index:int ->
    t ->
    ('a, 'b, [> `Rgba | `Rgb | `Gray ]) Bimage.Image.t ->
    (unit, error) result

  val read :
    ?index:int ->
    t ->
    ('a, 'b) Bimage.Type.t ->
    ([< `Rgba | `Rgb | `Gray ] as 'c) Bimage.Color.t ->
    (('a, 'b, 'c) Bimage.Image.t, error) result
end

module Output : sig
  type t

  val create : string -> (t, error) result

  val write :
    ?append:bool ->
    t ->
    ('a, 'b, [> `Rgb | `Rgba | `Gray ]) Bimage.Image.t ->
    (unit, error) result
end

val read :
  ('a, 'b) Bimage.Type.t ->
  ([< `Rgba | `Rgb | `Gray ] as 'c) Bimage.Color.t ->
  string ->
  (('a, 'b, 'c) Bimage.Image.t, error) result

val write :
  string ->
  ('a, 'b, [< `Rgba | `Rgb | `Gray ]) Bimage.Image.t ->
  (unit, error) result
