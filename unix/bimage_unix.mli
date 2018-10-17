open Bimage

(** Magick defines image I/O operations using ImageMagick/GraphicsMagick on the
    command-line *)
module Magick : sig
  val use_graphicsmagick : unit -> unit
  (* Use GraphicsMagick instead of ImageMagick *)

  val read :
       string
    -> ?format:string
    -> ?create:(   string
                -> ?layout:Image.layout
                -> ('a, 'b) kind
                -> 'c Color.t
                -> int
                -> int
                -> ('a, 'b, 'c) Image.t)
    -> ?layout:Image.layout
    -> ('a, 'b) kind
    -> ([< gray | rgb | rgba] as 'c) Color.t
    -> (('a, 'b, 'c) Image.t, Error.t) result
  (** [read filename kind color] loads an image from [filename] on disk using the given [kind] and [color] *)

  val write :
       ?quality:int
    -> string
    -> ?format:string
    -> ('a, 'b, [< gray | rgb | rgba]) Image.t
    -> unit
  (** [write filename image] saves an image to [filename] *)

  val read_all :
       string array
    -> ?format:string
    -> ?create:(   string
                -> ?layout:Image.layout
                -> ('a, 'b) kind
                -> 'c Color.t
                -> int
                -> int
                -> ('a, 'b, 'c) Image.t)
    -> ?layout:Image.layout
    -> ('a, 'b) kind
    -> ([< gray | rgb | rgba] as 'c) Color.t
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
  type t

  val frames : t -> int
  (** Get the number of frames for a video file *)

  val index : t -> int
  (** Get the current frame index for a video file *)

  val shape : t -> int * int
  (** Get the width and height of a video file *)

  val skip : t -> int -> unit
  (** Skip frames *)

  val set_index : t -> int -> unit
  (** Set the frame index *)

  val load : string -> t
  (** Open a video file *)

  val reset : t -> unit
  (** Reset the frame index to 0 *)

  val next :
       ?create:(   string
                -> ?layout:Image.layout
                -> int
                -> int
                -> (int, u8, rgb) Image.t)
    -> ?layout:Image.layout
    -> t
    -> (int, u8, rgb) Image.t option

  (** Get the next frame *)

  val read_n :
       t
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

module Data: sig
  val create_mmap: ('a, 'b) kind -> filename:string -> int -> ('a, 'b) Data.t
end

module Image: sig
  val create_mmap: ?layout:Image.layout -> ('a, 'b) kind -> 'c Color.t -> filename:string -> int -> int -> ('a, 'b, 'c) Image.t
end
