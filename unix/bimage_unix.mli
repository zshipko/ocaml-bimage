open Bimage

val read_u8: 'a Color.t -> string -> ((int, u8, 'a) Image.t, Error.t) result
val read_u16: 'a Color.t -> string -> ((int, u16, 'a) Image.t, Error.t) result
val read_f32: 'a Color.t -> string -> ((float, f32, 'a) Image.t, Error.t) result
val read: ('a, 'b) kind -> 'c Color.t -> string -> (('a, 'b, 'c) Image.t , Error.t) result

val write_png: string -> (int, u8, 'c) Image.t -> (unit, Error.t) result
val write_jpg: ?quality:int -> string -> (int, u8, 'c) Image.t -> (unit, Error.t) result
val write_hdr: string -> (float, f32, 'c) Image.t -> (unit, Error.t) result
val write: string -> ('a, 'b, 'c) Image.t -> (unit, Error.t) result

(** Magick defines image I/O operations using ImageMagick/GraphicsMagick on the
    command-line *)
module Magick : sig
  val use_graphicsmagick : unit -> unit
  (* Use GraphicsMagick instead of ImageMagick *)

  val read :
      ?create:(   string
                -> ?layout:Image.layout
                -> ('a, 'b) kind
                -> 'c Color.t
                -> int
                -> int
                -> ('a, 'b, 'c) Image.t)
    -> ?layout:Image.layout
    -> ('a, 'b) kind
    -> ([< gray | rgb | rgba] as 'c) Color.t
    -> ?format:string
    -> string
    -> (('a, 'b, 'c) Image.t, Error.t) result
  (** [read filename kind color] loads an image from [filename] on disk using the given [kind] and [color] *)

  val write :
       ?quality:int
    -> ?format:string
    -> string
    -> ('a, 'b, [< gray | rgb | rgba]) Image.t
    -> unit
  (** [write filename image] saves an image to [filename] *)

  val read_all :
      ?create:(   string
                -> ?layout:Image.layout
                -> ('a, 'b) kind
                -> 'c Color.t
                -> int
                -> int
                -> ('a, 'b, 'c) Image.t)
    -> ?layout:Image.layout
    -> ('a, 'b) kind
    -> ([< gray | rgb | rgba] as 'c) Color.t
    -> ?format:string
    -> string array
    -> (('a, 'b, 'c) Input.t, Error.t) result
  (** Read multiple images directly into an Input array *)

  val convert_command : string ref
  (** [convert_command] contains the command used to call out to ImageMagick/GraphicsMagick. For example,
      if you'd like to use GraphicsMagick then set this to "gm convert" *)

  val identify_command : string ref
  (** [identify_command] contains the command used to get information about image dimensions. It defaults to [itentify]
   *  but if you'd like to use GraphicsMagick then set this to "gm identify" *)
end

(** Ffmpeg is used to load images from video files. The [ffmpeg] command line tool is required *)
module Ffmpeg : sig
  (** Video file *)
  type input
  type output

  val frames : input -> int
  (** Get the number of frames for a video file *)

  val index : input -> int
  (** Get the current frame index for a video file *)

  val shape : input -> int * int
  (** Get the width and height of a video file *)

  val skip : input -> int -> unit
  (** Skip frames *)

  val set_index : input -> int -> unit
  (** Set the frame index *)

  val load : string -> input
  (** Open a video file *)

  val create :
    ?framerate:int ->
    string ->
    int -> int -> output

  val write_frame :
    output -> (int, u8, rgb) Image.t -> unit

  val finish : output -> unit

  val reset : input -> unit
  (** Reset the frame index to 0 *)

  val next :
       ?create:(   string
                -> ?layout:Image.layout
                -> int
                -> int
                -> (int, u8, rgb) Image.t)
    -> ?layout:Image.layout
    -> input
    -> (int, u8, rgb) Image.t option

  (** Get the next frame *)

  val read_n :
       input
    -> ?create:(   string
                -> ?layout:Image.layout
                -> int
                -> int
                -> (int, u8, rgb) Image.t)
    -> int
    -> (int, u8, rgb) Input.t
  (** Read multiple images directly into an Input array. The resulting array will not contain the number of frames requested
      if you've reached the end of the video *)
end

module Data_unix: sig
  val create_mmap: ('a, 'b) kind -> filename:string -> int -> ('a, 'b) Data.t
end

module Image_unix: sig
  val create_mmap: ?layout:Image.layout -> ('a, 'b) kind -> 'c Color.t -> filename:string -> int -> int -> ('a, 'b, 'c) Image.t
end

