open Bimage

type input

type output

type spec

type error = [ Error.t | `File_not_found of string ]

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

let base_type_of_ty : type a b. (a, b) Type.t -> base_type =
  fun (module T) ->
  match T.kind with
  | Float64 -> Double
  | Float32 -> Float
  | Int8_unsigned -> UInt8
  | Int16_unsigned -> UInt16
  | Int32 -> Int32
  | Int64 -> Int64
  | _ -> raise Unsupported

external image_spec : int -> int -> int -> base_type -> spec = "image_spec"

let make_spec ty color width height =
  let base = base_type_of_ty ty in
  image_spec width height (Color.channels color) base

external spec_shape : spec -> int * int * int = "spec_shape"

external spec_base_type : spec -> base_type = "spec_base_type"

external input_open : string -> input = "input_open"

external input_get_spec : input -> spec = "input_get_spec"

external input_read :
  input -> channels:int -> index:int -> spec -> ('a, 'b) Data.t -> unit
  = "input_read"

external output_create : string -> output = "output_create"

external output_open : output -> string -> spec -> bool -> unit = "output_open"

external output_write_image : output -> spec -> ('a, 'b) Data.t -> unit
  = "output_write_image"

module Spec = struct
  type t = spec

  let shape t = spec_shape t

  let base_type t = spec_base_type t
end

module Input = struct
  type t = input

  let init filename =
    try Ok (input_open filename) with Failure reason -> Error (`Msg reason)

  let spec input = input_get_spec input

  let read_image ?(index = 0) input image =
    try
      let w, h, _c = Image.shape image in
      let spec = make_spec (Image.ty image) (Image.color image) w h in
      Ok
        (input_read input ~channels:(Image.channels image) ~index spec
           (Image.data image))
    with Failure reason -> Error (`Msg reason)

  let read ?index input ty color =
    let spec = spec input in
    let width, height, channels = Spec.shape spec in
    if channels > Color.channels color then Error `Invalid_color
    else
      let image = Image.create ty color width height in
      match read_image ?index input image with
      | Ok () -> Ok image
      | Error e -> Error e
end

module Output = struct
  type t = string * output

  let create filename =
    try Ok (filename, output_create filename)
    with Failure reason -> Error (`Msg reason)

  let write ?(append = false) (filename, output) image =
    try
      let spec =
        make_spec (Image.ty image) image.color image.width image.height
      in
      let () = output_open output filename spec append in
      let () = output_write_image output spec (Image.data image) in
      Ok ()
    with Failure reason -> Error (`Msg reason)
end

let write filename image =
  match Output.create filename with
  | Ok output -> Output.write output image
  | Error e -> Error e

let read t c filename =
  match Input.init filename with
  | Ok input -> Input.read input t c
  | Error e -> Error e
