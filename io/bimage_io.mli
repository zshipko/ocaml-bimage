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
  type 'a attr = Int : int attr | Float : float attr | String : string attr

  val shape : t -> int * int * int
  val base_type : t -> base_type
  val make : ('a, 'b) Bimage.Type.t -> 'c Bimage.Color.t -> int -> int -> t
  val get_attr : t -> string -> 'a attr -> 'a option
  val set_attr : t -> string -> 'a attr -> 'a -> unit
  val attr_names : t -> string array
end

module Input : sig
  type t

  val init : string -> (t, error) result
  val spec : t -> Spec.t

  val read_image :
    ?index:int -> t -> ('a, 'b, [ `Rgb ]) Bimage.Image.t -> (unit, error) result

  val read :
    ?index:int ->
    t ->
    ('a, 'b) Bimage.Type.t ->
    ([ `Rgb ] as 'c) Bimage.Color.t ->
    (('a, 'b, 'c) Bimage.Image.t, error) result
end

module Output : sig
  type t

  val create : string -> (t, error) result

  val write :
    ?spec:Spec.t ->
    ?append:bool ->
    t ->
    ('a, 'b, [< `Rgb | `Rgba | `Gray ]) Bimage.Image.t ->
    (unit, error) result
end

val read :
  ('a, 'b) Bimage.Type.t ->
  'c Bimage.Color.t ->
  string ->
  (('a, 'b, 'c) Bimage.Image.t, error) result

val write :
  string ->
  ('a, 'b, [< `Rgba | `Rgb | `Gray ]) Bimage.Image.t ->
  (unit, error) result
