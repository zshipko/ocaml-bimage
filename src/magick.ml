open Type
open Color

let pixel_type: [< gray|rgb|rgba] Color.t -> string = fun c -> match c.t with
  | `Gray -> "gray"
  | `Rgb -> "rgb"
  | `Rgba -> "rgba"

let interlace = function
  | Image.Planar -> "Plane"
  | Image.Interleaved -> "None"

let convert_command = ref "convert"
let identify_command = ref "identify"

let use_graphicsmagick () =
  convert_command := "gm convert";
  identify_command := "gm identify"

let read filename ?(create = fun _name -> Image.create ?mmap:None) ?(layout = Image.Interleaved) t color =
  try
    let read_image_data filename img =
      let f = pixel_type img.Image.color in
      let channels = Image.channels img in
      let interlace = interlace layout in
      let cmd = Printf.sprintf "%s '%s' -depth 8 -interlace %s %s:-" !convert_command filename interlace f in
      let input = Unix.open_process_in cmd in
      let kind = Kind.of_float t in
      for i = 0 to (Image.(img.width *  img.height)  * channels) - 1 do
        let x = Kind.to_float u8 (input_byte input) in
        let x = Kind.normalize u8 x in
        let x = Kind.denormalize t x in
        img.Image.data.{i} <- kind x
      done;
      close_in input
    in
    (* Read image size *)
    let cmd = Printf.sprintf "%s -format \"%%w %%h\" '%s'" !identify_command filename in
    let identify = Unix.open_process_in cmd in
    let s = input_line identify in
    let () = close_in identify in
    let shape =
      String.split_on_char ' ' (String.trim s)
    in
    match List.map int_of_string shape with
    | x::y::[] ->
        let img = create filename ~layout t color x y in
        let () = read_image_data filename img in
        Ok img
    | _ -> Error (`Invalid_shape)
  with End_of_file -> Error `Invalid_shape | Failure msg -> Error (`Msg msg)

let read_all filenames ?create ?layout kind color =
  try
    Ok (Array.map (fun f -> read f ?create ?layout kind color |> Error.unwrap) filenames)
  with Error.Exc err -> Error err

let write ?quality filename img =
  let width, height, channels = Image.shape img in
  let f = pixel_type img.Image.color in
  let quality = match quality with None -> "" | Some q -> Printf.sprintf "-quality %d" q in
  let cmd = Printf.sprintf "%s -depth 8 -interlace %s -size %dx%d %s:- %s '%s'" !convert_command (interlace img.layout) width height f quality filename in
  let output = Unix.open_process_out cmd in
  let kind = Image.kind img in
  for i = 0 to Image.(img.width *  img.height)  * channels - 1 do
    let x = Kind.to_float kind img.Image.data.{i} in
    let x = Kind.normalize kind x in
    let x = Kind.denormalize u8 x in
    output_byte output (Kind.of_float u8 x)
  done;
  close_out output

