open Bimage

let convert_command = ref "convert"

let identify_command = ref "identify"

let read (type color) ?(create = fun _name -> Image.v) t
    (module C : COLOR with type t = color) ?format filename =
  let format =
    match format with
    | Some f -> f
    | None ->
        Filename.extension filename |> fun s ->
        String.sub s 1 (String.length s - 1)
  in
  try
    let read_image_data filename img =
      let f = C.name C.t in
      let channels = Image.channels img in
      let cmd =
        Printf.sprintf "%s %s:'%s' -depth 8 %s:-" !convert_command format
          filename f
      in
      let input = Unix.open_process_in cmd in
      for i = 0 to (Image.(img.width * img.height) * channels) - 1 do
        let x = Type.to_float u8 (input_byte input) in
        let x = Type.normalize u8 x in
        let x = Type.denormalize t x in
        img.Image.data.{i} <- Type.of_float t x
      done;
      close_in input
    in
    (* Read image size *)
    let cmd =
      Printf.sprintf "%s -format \"%%w %%h\" '%s'" !identify_command filename
    in
    let identify = Unix.open_process_in cmd in
    let s = input_line identify in
    let () = close_in identify in
    let shape = String.split_on_char ' ' (String.trim s) in
    match List.map int_of_string shape with
    | [ x; y ] ->
        let img = create filename t (module C) x y in
        let () = read_image_data filename img in
        Ok img
    | _ -> Error `Invalid_shape
  with
  | End_of_file -> Error (`Msg "end of file")
  | Failure msg -> Error (`Msg msg)

let read_all ?create kind color ?format filenames =
  try
    Ok
      (Array.map
         (fun f ->
           read ?create kind color ?format f |> Error.unwrap |> Image.any)
         filenames)
  with Error.Exc err -> Error err

let write (type color) ?quality ?format filename img =
  let format =
    match format with
    | Some f -> f
    | None ->
        Filename.extension filename |> fun s ->
        String.sub s 1 (String.length s - 1)
  in
  let width, height, channels = Image.shape img in
  let (module C : COLOR with type t = color) = img.Image.color in
  let f = C.name C.t in
  let quality =
    match quality with None -> "" | Some q -> Printf.sprintf "-quality %d" q
  in
  let kind = Image.ty img in
  let _depth = Type.depth kind in
  let cmd =
    Printf.sprintf "%s -size %dx%d -depth 8 %s:- %s %s:'%s'" !convert_command
      width height f quality format filename
  in
  let output = Unix.open_process_out cmd in
  for i = 0 to (Image.(img.width * img.height) * channels) - 1 do
    let x = Type.to_float kind img.Image.data.{i} in
    let x = Type.normalize kind x in
    let x = Type.denormalize u8 x in
    let x = Type.of_float u8 x in
    output_byte output x
  done;
  close_out output
