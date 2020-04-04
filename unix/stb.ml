open Ctypes
open Foreign

external _stbi_load_stub: unit -> unit = "stbi_load"
external _stbi_load_stub: unit -> unit = "stbi_load_16"
external _stbi_load_stub: unit -> unit = "stbi_loadf"

external _stbi_write_png: unit -> unit = "stbi_write_png"
external _stbi_write_jpg: unit -> unit = "stbi_write_jpg"
external _stbi_write_hdr: unit -> unit = "stbi_write_hdr"

let free = foreign "free" (ptr void @-> returning void)

let stbi_load_u8 =
  foreign ~release_runtime_lock:true "stbi_load"
    (string @-> ptr int @-> ptr int @-> ptr int @-> int @-> returning (ptr uint8_t))


let stbi_load_u16 =
  foreign ~release_runtime_lock:true "stbi_load_16"
    (string @-> ptr int @-> ptr int @-> ptr int @-> int @-> returning (ptr uint16_t))

let stbi_load_f =
  foreign ~release_runtime_lock:true "stbi_loadf"
    (string @-> ptr int @-> ptr int @-> ptr int @-> int @-> returning (ptr float))

let read f a b c color filename =
  let width = allocate int 0 in
  let height = allocate int 0 in
  let channels = allocate int 0 in
  let n = Bimage.Color.channels color in
  let data = f filename width height channels n in
  if is_null data then Error (`Msg (Printf.sprintf "unable to open image: %s" filename))
  else
    let data = coerce  (ptr a) (ptr b) data in
    let data' = Ctypes.bigarray_of_ptr array1 (!@width * !@height * !@channels) c data in
    let im = Bimage.Image.of_data color (!@width) (!@height) Bimage.Image.Interleaved data' in
    let () = Gc.finalise (fun _ -> free (coerce (ptr b) (ptr void) data)) im in
    Ok im

let read_u8 color filename =
  read stbi_load_u8 uint8_t int Bimage.u8 color filename

let read_u16 color filename =
  read stbi_load_u16 uint16_t int Bimage.u16 color filename

let read_f32 color filename =
  read stbi_load_f float float Bimage.f32 color filename

let read kind color filename =
  match read_u16 color filename with
  | Error e -> Error e
  | Ok tmp ->
    Ok (Bimage.Image.convert kind tmp)

let stbi_write_png =
  foreign ~release_runtime_lock:true "stbi_write_png"
    (string @-> int @-> int @-> int @-> ptr int @-> int @-> returning int)

let stbi_write_jpg =
  foreign ~release_runtime_lock:true "stbi_write_jpg"
    (string @-> int @-> int @-> int @-> ptr int @-> int @-> returning int)

let stbi_write_hdr =
  foreign ~release_runtime_lock:true "stbi_write_hdr"
    (string @-> int @-> int @-> int @-> ptr float @-> returning int)

let write_png filename image =
  let image = match image.Bimage.Image.layout with
    | Planar -> Bimage.Image.convert_layout Interleaved image
    | Interleaved -> image
  in
  let width, height, channels = Bimage.Image.shape image in
  let ptr = Ctypes.bigarray_start array1 (Bimage.Image.data image) in
  if stbi_write_png filename width height channels ptr (width * channels) = 0 then
    Error (`Msg (Printf.sprintf "unable to load image: %s" filename))
  else Ok ()

let write_jpg ?(quality = 95) filename image =
  let image = match image.Bimage.Image.layout with
    | Planar -> Bimage.Image.convert_layout Interleaved image
    | Interleaved -> image
  in
  let width, height, channels = Bimage.Image.shape image in
  let ptr = Ctypes.bigarray_start array1 (Bimage.Image.data image) in
  if stbi_write_jpg filename width height channels ptr quality = 0 then
    Error (`Msg (Printf.sprintf "unable to load image: %s" filename))
  else Ok ()

let write_hdr filename image =
  let image = match image.Bimage.Image.layout with
    | Planar -> Bimage.Image.convert_layout Interleaved image
    | Interleaved -> image
  in
  let width, height, channels = Bimage.Image.shape image in
  let ptr = Ctypes.bigarray_start array1 (Bimage.Image.data image) in
  if stbi_write_hdr filename width height channels ptr = 0 then
    Error (`Msg (Printf.sprintf "unable to load image: %s" filename))
  else Ok ()

let write filename image =
  match Filename.extension filename |> String.lowercase_ascii with
  | ".png" ->
    let tmp = Bimage.Image.convert Bimage.u8 image in
    write_png filename tmp
  | ".jpeg" | ".jpg" ->
    let tmp = Bimage.Image.convert Bimage.u8 image in
    write_jpg filename tmp
  | ".hdr" ->
    let tmp = Bimage.Image.convert Bimage.f32 image in
    write_hdr filename tmp
  | ext -> Error (`Msg (Printf.sprintf "invalid file extension for writing image: %s" ext))
