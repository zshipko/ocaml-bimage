open Bimage

(** Stb contains image I/O operationgs using stb_image *)
module Stb : sig
  val read_u8 : 'a Color.t -> string -> ((int, u8, 'a) Image.t, Error.t) result

  val read_u8_from_memory :
    'a Color.t -> bytes -> ((int, u8, 'a) Image.t, Error.t) result

  val read_u16 :
    'a Color.t -> string -> ((int, u16, 'a) Image.t, Error.t) result

  val read_u16_from_memory :
    'a Color.t -> bytes -> ((int, u16, 'a) Image.t, Error.t) result

  val read_f32 :
    'a Color.t -> string -> ((float, f32, 'a) Image.t, Error.t) result

  val read_f32_from_memory :
    'a Color.t -> bytes -> ((float, f32, 'a) Image.t, Error.t) result

  val read :
    ('a, 'b) ty ->
    'c Color.t ->
    string ->
    (('a, 'b, 'c) Image.t, Error.t) result

  val read_from_memory :
    ('a, 'b) ty ->
    'c Color.t ->
    bytes ->
    (('a, 'b, 'c) Image.t, Error.t) result

  val write_png : string -> (int, u8, 'c) Image.t -> (unit, Error.t) result

  val write_jpg :
    ?quality:int -> string -> (int, u8, 'c) Image.t -> (unit, Error.t) result

  val write_hdr : string -> (float, f32, 'c) Image.t -> (unit, Error.t) result

  val write : string -> ('a, 'b, 'c) Image.t -> (unit, Error.t) result
end

(** Magick contains image I/O operations using ImageMagick/GraphicsMagick on the
    command-line *)
module Magick : sig
  val use_graphicsmagick : unit -> unit

  (* Use GraphicsMagick instead of ImageMagick *)

  val read :
    ?create:
      (string ->
      ('a, 'b) ty ->
      'c Color.t ->
      int ->
      int ->
      ('a, 'b, 'c) Image.t) ->
    ('a, 'b) ty ->
    ([< `Gray | `Rgb | `Rgba ] as 'c) Color.t ->
    ?format:string ->
    string ->
    (('a, 'b, 'c) Image.t, Error.t) result
  (** [read filename kind color] loads an image from [filename] on disk using the given [kind] and [color] *)

  val write :
    ?quality:int ->
    ?format:string ->
    string ->
    ('a, 'b, [< `Gray | `Rgb | `Rgba]) Image.t ->
    unit
  (** [write filename image] saves an image to [filename] *)

  val read_all :
    ?create:
      (string ->
      ('a, 'b) ty ->
      'c Color.t ->
      int ->
      int ->
      ('a, 'b, 'c) Image.t) ->
    ('a, 'b) ty ->
    ([< `Gray | `Rgb | `Rgba] as 'c) Color.t ->
    ?format:string ->
    string array ->
    (('a, 'b, 'c) Input.t, Error.t) result
  (** Read multiple images directly into an Input array *)

  val convert_command : string ref
  (** [convert_command] contains the command used to call out to ImageMagick/GraphicsMagick. For example,
      if you'd like to use GraphicsMagick then set this to "gm convert" *)

  val identify_command : string ref
  (** [identify_command] contains the command used to get information about image dimensions. It defaults to [itentify]
   *  but if you'd like to use GraphicsMagick then set this to "gm identify" *)
end

module Data_unix : sig
  val create_mmap :
    ?mode:int -> ('a, 'b) ty -> filename:string -> int -> ('a, 'b) Data.t
end

module Image_unix : sig
  val create_mmap :
    ?mode:int ->
    ('a, 'b) ty ->
    'c Color.t ->
    filename:string ->
    int ->
    int ->
    ('a, 'b, 'c) Image.t
end
