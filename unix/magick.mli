open Bimage

val read :
  ?create:
    (string ->
    ('a, 'b) Type.t ->
    'c Color.t ->
    int ->
    int ->
    ('a, 'b, 'c) Image.t) ->
  ('a, 'b) Type.t ->
  ([< `Gray | `Rgb | `Rgba ] as 'c) Color.t ->
  ?format:string ->
  string ->
  (('a, 'b, 'c) Image.t, Error.t) result
(** [read filename kind color] loads an image from [filename] on disk using the given [kind] and [color] *)

val write :
  ?quality:int ->
  ?format:string ->
  string ->
  ('a, 'b, [< `Gray | `Rgb | `Rgba ]) Image.t ->
  unit
(** [write filename image] saves an image to [filename] *)

val read_all :
  ?create:
    (string ->
    ('a, 'b) Type.t ->
    'c Color.t ->
    int ->
    int ->
    ('a, 'b, 'c) Image.t) ->
  ('a, 'b) Type.t ->
  ([< `Gray | `Rgb | `Rgba ] as 'c) Color.t ->
  ?format:string ->
  string array ->
  (Input.t, Error.t) result
(** Read multiple images directly into an Input array *)

val convert_command : string ref
(** [convert_command] contains the command used to call out to ImageMagick/GraphicsMagick. For example,
      if you'd like to use GraphicsMagick then set this to "gm convert" *)

val identify_command : string ref
(** [identify_command] contains the command used to get information about image dimensions. It defaults to [itentify]
   *  but if you'd like to use GraphicsMagick then set this to "gm identify" *)
