open Bigarray

module type TYPE = sig
  type t
  type elt
  type kind = (t, elt) Bigarray.kind

  val name: string
  val kind: kind
  val of_float: float -> t
  val to_float: t -> float
end

type ('a, 'b) kind = ('a, 'b) Bigarray.kind
type ('a, 'b) t = (module TYPE with type t = 'a and type elt = 'b)

exception Unsupported

module U8 = struct
  type t = int
  type elt = int8_unsigned_elt
  type kind = (t, elt) Bigarray.kind

  let name = "u8"
  let kind = Int8_unsigned
  let of_float = int_of_float
  let to_float = float_of_int
end

module U16 = struct
  type t = int
  type elt = int16_unsigned_elt
  type kind = (t, elt) Bigarray.kind

  let name = "u16"
  let kind = Int16_unsigned
  let of_float = int_of_float
  let to_float = float_of_int
end

module F32 = struct
  type t = float
  type elt = float32_elt
  type kind = (t, elt) Bigarray.kind

  let name = "f32"
  let kind = Float32
  let of_float x = x
  let to_float x = x
end

module F64 = struct
  type t = float
  type elt = float64_elt
  type kind = (t, elt) Bigarray.kind

  let name = "f64"
  let kind = Float64
  let of_float x = x
  let to_float x = x
end

module I32 = struct
  type t = int32
  type elt = int32_elt
  type kind = (t, elt) Bigarray.kind

  let name = "i32"
  let kind = Int32
  let of_float = Int32.of_float
  let to_float = Int32.to_float
end

module I64 = struct
  type t = int64
  type elt = int64_elt
  type kind = (t, elt) Bigarray.kind

  let name = "i64"
  let kind = Int64
  let of_float = Int64.of_float
  let to_float = Int64.to_float
end


type u8_elt = int8_unsigned_elt
type u8 = (module TYPE with type t = int and type elt = u8_elt)

type u16_elt = int16_unsigned_elt
type u16 = (module TYPE with type t = int and type elt = u16_elt)

type i32_elt = int32_elt
type i32 = (module TYPE with type t = int32 and type elt = i32_elt)

type i64_elt = int64_elt
type i64 = (module TYPE with type t = int64 and type elt = i64_elt)

type f32_elt = float32_elt
type f32 = (module TYPE with type t = float and type elt = f32_elt)

type f64_elt = float64_elt
type f64 = (module TYPE with type t = float and type elt = f64_elt)

let u8: u8 = (module U8)

let u16: u16 = (module U16)

let i32: i32 = (module I32)

let i64: i64 = (module I64)

let f32: f32 = (module F32)

let f64: f64 = (module F64)

let name : type a b. (a, b) t -> string = fun (module T) ->
  T.name

let depth : type a b. (a, b) t -> int = fun (module T) ->
  match T.kind with
  | Int8_unsigned -> 8
  | Int16_unsigned -> 16
  | Int32 -> 32
  | Int64 -> 64
  | Float32 -> 32
  | Float64 -> 32
  | _ -> raise Unsupported

let[@inline] max : type a b. (a, b) t -> a = fun (module T) ->
  match T.kind with
  | Int8_unsigned -> 255
  | Int16_unsigned -> 65535
  | Int32 -> Int32.max_int
  | Int64 -> Int64.max_int
  | Float32 -> 1.0
  | Float64 -> 1.0
  | _ -> raise Unsupported

let[@inline] min : type a b. (a, b) t -> a = fun (module T) ->
  match T.kind with
  | Int8_unsigned -> 0
  | Int16_unsigned -> 0
  | Int32 -> Int32.min_int
  | Int64 -> Int64.min_int
  | Float32 -> 0.0
  | Float64 -> 0.0
  | _ -> raise Unsupported

let[@inline] max_f : type a b. (a, b) t -> float = fun (module T) ->
  match T.kind with
  | Int8_unsigned -> 255.
  | Int16_unsigned -> 65535.
  | Int32 -> Int32.max_int |> Int32.to_float
  | Int64 -> Int64.max_int |> Int64.to_float
  | Float32 -> 1.0
  | Float64 -> 1.0
  | _ -> raise Unsupported

let[@inline] min_f : type a b. (a, b) t -> float = fun (module T) ->
  match T.kind with
  | Int8_unsigned -> 0.0
  | Int16_unsigned -> 0.0
  | Int32 -> Int32.min_int |> Int32.to_float
  | Int64 -> Int64.min_int |> Int64.to_float
  | Float32 -> 0.0
  | Float64 -> 0.0
  | _ -> raise Unsupported

let[@inline] to_float : type a b. (a, b) t -> a -> float =
 fun (module T) v ->
   T.to_float v

let[@inline] of_float : type a b. (a, b) t -> float -> a =
 fun (module T) v ->
   T.of_float v

let clamp kind f =
  let min = min_f kind in
  let max = max_f kind in
  if f < min then min else if f > max then max else f

let normalize kind f =
  let min = min_f kind in
  let max = max_f kind in
  (f -. min) /. (max -. min)

let denormalize kind f =
  let min = min_f kind in
  let max = max_f kind in
  f *. (max -. min)

let convert ~from to_kind f =
  to_float from f |> normalize from |> denormalize to_kind |> of_float to_kind

let kind : type a b. (a, b) t -> (a, b) Bigarray.kind = fun (module T) -> T.kind

let of_kind (type a b) : (a, b) Bigarray.kind -> (a, b) t = function
   | Int8_unsigned -> u8
   | Int16_unsigned -> u16
   | Int32 -> i32
   | Int64 -> i64
   | Float32 -> f32
   | Float64 -> f64
   | _ -> raise Unsupported
