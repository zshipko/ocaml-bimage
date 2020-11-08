open Bimage

type input
type output
type spec

type error = [ Error.t | `File_not_found of string]

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

let base_type_of_ty : type a b. (a, b) Type.t -> base_type = fun (module T) ->
  match T.kind with
  | Float64 -> Double
  | Float32 -> Float
  | Int8_unsigned -> UInt8
  | Int16_unsigned -> UInt16
  | Int32 -> Int32
  | Int64 -> Int64
  | _ -> raise Unsupported

external image_spec: int -> int -> int -> base_type -> spec = "image_spec"

let image_spec (type color) ty (module C: COLOR with type t = color) width height =
  let base = base_type_of_ty ty in
  image_spec width height (C.channels C.t) base

external spec_shape: spec -> int * int * int = "spec_shape"
external spec_base_type : spec -> base_type = "spec_base_type"

external input_open: string -> input = "input_open"
external input_get_spec: input -> spec = "input_get_spec"
external input_read: input -> int -> ('a, 'b) Data.t -> unit = "input_read"

external output_create: string -> output = "output_create"
external output_set_spec: output -> spec -> unit = "output_open"
external output_write_image: output -> ('a, 'b) Data.t -> unit = "output_write_image"

module Spec = struct
  type t = spec

  let shape t = spec_shape t
  let base_type t = spec_base_type t
end

module Input = struct
  type t = input

  let init filename =
    try
      Ok (input_open filename)
    with Failure reason -> Error (`Msg reason)

  let spec input = input_get_spec input

  let read input image =
    try
       Ok (input_read input (Image.channels image) (Image.data image))
    with Failure reason -> Error (`Msg reason)

  let read_image input ty (module C: COLOR) =
    let spec = spec input in
    let (width, height, channels) = Spec.shape spec in
    if channels > C.channels C.t then
      Error `Invalid_color
    else
      let image = Image.create ty (module C) width height in
      read input image
end

module Output = struct
  type t = output

  let create filename =
    try
      Ok (output_create filename)
    with Failure reason -> Error (`Msg reason)

  let set_spec output s =
    try
      Ok (output_set_spec output s)
    with Failure reason -> Error (`Msg reason)

  let write output image =
    try
        let () = output_write_image output (Image.data image) in
        Ok ()
    with Failure reason -> Error (`Msg reason)

  let write_image output image =
    try
      let spec = image_spec (Image.ty image) image.color image.width image.height in
        let () = output_set_spec output spec in
        let () = output_write_image output (Image.data image) in
        Ok ()
    with Failure reason -> Error (`Msg reason)
end
