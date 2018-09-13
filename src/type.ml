open Bigarray

type ('a, 'b) kind = ('a, 'b) Bigarray.kind

exception Unsupported

type u8 = int8_unsigned_elt
type u16 = int16_unsigned_elt
type i32 = int32_elt
type i64 = int64_elt
type f32 = float32_elt
type f64 = float64_elt
type c32 = complex32_elt
type c64 = complex64_elt

let u8 = Int8_unsigned
let u16 = Int16_unsigned
let i32 = Int32
let i64 = Int64
let f32 = Float32
let f64 = Float64
let c32 = Complex32
let c64 = Complex64

module Kind = struct
  let max: type a b. (a, b) kind -> a = function
    | Int8_unsigned -> 255
    | Int16_unsigned -> 65535
    | Int32 -> Int32.max_int
    | Int64 -> Int64.max_int
    | Float32 -> 1.0
    | Float64 -> 1.0
    | Complex32 -> Complex.one
    | Complex64 -> Complex.one
    | _ -> raise Unsupported
  [@@inline]

  let min: type a b. (a, b) kind -> a = function
    | Int8_unsigned -> 0
    | Int16_unsigned -> 0
    | Int32 -> Int32.min_int
    | Int64 -> Int64.min_int
    | Float32 -> 0.0
    | Float64 -> 0.0
    | Complex32 -> Complex.zero
    | Complex64 -> Complex.zero
    | _ -> raise Unsupported
  [@@inline]

  let max_f: type a b. (a, b) kind -> float = function
    | Int8_unsigned -> 255.
    | Int16_unsigned -> 65535.
    | Int32 -> Int32.max_int |> Int32.to_float
    | Int64 -> Int64.max_int |> Int64.to_float
    | Float32 -> 1.0
    | Float64 -> 1.0
    | Complex32 -> 1.0
    | Complex64 -> 1.0
    | _ -> raise Unsupported
  [@@inline]

  let min_f: type a b. (a, b) kind -> float = function
    | Int8_unsigned -> 0.0
    | Int16_unsigned -> 0.0
    | Int32 -> Int32.min_int |> Int32.to_float
    | Int64 -> Int64.min_int |> Int64.to_float
    | Float32 -> 0.0
    | Float64 -> 0.0
    | Complex32 -> 0.0
    | Complex64 -> 0.0
    | _ -> raise Unsupported
  [@@inline]

  let to_float: type a b. (a, b) kind -> a -> float = fun kind v ->
    match kind with
    | Int8_unsigned -> float_of_int v
    | Int16_unsigned -> float_of_int v
    | Int32 -> Int32.to_float v
    | Int64 -> Int64.to_float v
    | Float32 -> v
    | Float64 -> v
    | Complex32 -> Complex.norm v
    | _ -> raise Unsupported
  [@@inline]

  let of_float: type a b. (a, b) kind -> float -> a = fun kind v ->
    match kind with
    | Int8_unsigned -> int_of_float v
    | Int16_unsigned -> int_of_float v
    | Int32 -> Int32.of_float v
    | Int64 -> Int64.of_float v
    | Float32 -> v
    | Float64 -> v
    | Complex32 -> Complex.{re = v; im = 0.}
    | _ -> raise Unsupported
  [@@inline]

  let clamp kind f =
    let min = min_f kind in
    let max = max_f kind in
    if f < min then min
    else if f > max then max
    else f

  let normalize kind f =
    let min = min_f kind in
    let max = max_f kind in
    (f -.  min) /. (max -. min)

  let denormalize kind f =
    let min = min_f kind in
    let max = max_f kind in
    f *. (max -. min)

  let convert ~from to_kind f =
    to_float from f
    |> normalize from
    |> denormalize to_kind
    |> of_float to_kind
end

