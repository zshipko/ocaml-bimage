open Bimage

type input
type output
type spec

type error =
  | Unsupoorted_color
  | Unsupported_type
  | Error of string
  | File_not_found of string

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

let base_type_of_ty : type a b. (a, b) ty -> (base_type, error) result = function
  | Float64 -> Ok Double
  | Float32 -> Ok Float
  | Int8_signed -> Ok Int8
  | Int16_signed -> Ok Int16
  | Int8_unsigned -> Ok UInt8
  | Int16_unsigned -> Ok UInt16
  | Int32 -> Ok Int32
  | Int64 -> Ok Int64
  | _ -> Error Unsupported_type

external image_spec: int -> int -> int -> base_type -> spec = "image_spec"

let image_spec (type color) ty (module C: COLOR with type t = color) width height =
  match base_type_of_ty ty with
  | Ok base -> Ok (image_spec width height (C.channels C.t) base)
  | Error e -> Error e

external input_open: string -> (input, error) result = "input_open"
external input_get_spec: input -> spec = "input_get_spec"
external input_read: input -> ('a, 'b, 'c) Image.t -> (unit, error) result = "input_read"

external output_create: string -> (output, error) result = "output_create"
external output_open: output -> spec -> (unit, error) result = "output_open"
external output_write_image: output -> ('a, 'b, 'c) Image.t -> (unit, error) result = "output_write_image"

module Input = struct
  type t = input
end

module Output = struct
  type t = output
end
