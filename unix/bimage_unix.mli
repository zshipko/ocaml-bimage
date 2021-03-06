open Bimage

module Thread : sig
  module Filter () : sig
    include Bimage.FILTER with type 'a io = 'a
  end
end

module Stb = Stb
(** Stb contains image I/O operationgs using stb_image *)

module Magick = Magick
(** Magick contains image I/O operations using ImageMagick/GraphicsMagick on the
    command-line *)

module Data_unix : sig
  val create_mmap :
    ?mode:int -> ('a, 'b) Type.t -> filename:string -> int -> ('a, 'b) Data.t
end

module Image_unix : sig
  val create_mmap :
    ?mode:int ->
    ('a, 'b) Type.t ->
    'c Color.t ->
    filename:string ->
    int ->
    int ->
    ('a, 'b, 'c) Image.t
end
