open Bimage

val read_u8 : 'a Color.t -> string -> ((int, u8, 'a) Image.t, Error.t) result

val read_u8_from_memory :
  'a Color.t -> bytes -> ((int, u8, 'a) Image.t, Error.t) result

val read_u16 : 'a Color.t -> string -> ((int, u16, 'a) Image.t, Error.t) result

val read_u16_from_memory :
  'a Color.t -> bytes -> ((int, u16, 'a) Image.t, Error.t) result

val read_f32 :
  'a Color.t -> string -> ((float, f32, 'a) Image.t, Error.t) result

val read_f32_from_memory :
  'a Color.t -> bytes -> ((float, f32, 'a) Image.t, Error.t) result

val read :
  ('a, 'b) Type.t ->
  'c Color.t ->
  string ->
  (('a, 'b, 'c) Image.t, Error.t) result

val read_from_memory :
  ('a, 'b) Type.t ->
  'c Color.t ->
  bytes ->
  (('a, 'b, 'c) Image.t, Error.t) result

val write_png : string -> (int, u8, 'c) Image.t -> (unit, Error.t) result

val write_jpg :
  ?quality:int -> string -> (int, u8, 'c) Image.t -> (unit, Error.t) result

val write_hdr : string -> (float, f32, 'c) Image.t -> (unit, Error.t) result

val write : string -> ('a, 'b, 'c) Image.t -> (unit, Error.t) result
