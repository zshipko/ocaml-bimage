open Bimage

type input =
  { filename : string
  ; width : int
  ; height : int
  ; frames : int
  ; mutable index : int }

let frames {frames; _} = frames

let index {index; _} = index

let shape {width; height; _} = (width, height)

let reset t = t.index <- 0

let get_num_frames filename =
  let cmd =
    "ffmpeg -i " ^ filename ^ " -map 0:v:0 -c copy -f null -y /dev/null 2>&1 | grep frame= | tail -n 1 | awk '{ print $2 }'"
  in
  let proc = Unix.open_process_in cmd in
  let frames = input_line proc in
  close_in proc; int_of_string frames


let get_size filename =
  let cmd =
    "ffprobe -v error -select_streams v:0 -show_entries stream=width,height \
     -of csv=s=x:p=0 "
    ^ filename
  in
  let proc = Unix.open_process_in cmd in
  let size = input_line proc in
  let shape = String.split_on_char 'x' size in
  match shape with
  | x :: y :: _ ->
      (int_of_string x, int_of_string y)
  | _ ->
      Error.exc `Invalid_shape

let load filename =
  let width, height = get_size filename in
  {filename; width; height; frames = get_num_frames filename; index = 0}


let set_index t f = t.index <- f

let skip t f = t.index <- t.index + f

let next
    ?(create =
      fun _name ?layout x y ->
        Image.create ?layout u8 Bimage.rgb x y) ?layout t =
  if t.index >= t.frames then None
  else
    try
      let cmd =
        Printf.sprintf
          "ffmpeg  -v error -hide_banner -i %s -vf 'select=gte(n\\,%d)' \
           -vframes 1 -pix_fmt rgb24 -f rawvideo -an -"
          t.filename t.index
      in
      let proc = Unix.open_process_in cmd in
      let img =
        create
          (Printf.sprintf "%s-%d" t.filename t.index)
          ?layout t.width t.height
      in
      Image.for_each
        (fun x y _px ->
          for i = 0 to Image.channels img - 1 do
            Image.set img x y i @@ input_byte proc
          done )
        img;
      close_in proc;
      t.index <- t.index + 1;
      Some img
    with _ -> None


let read_n t ?create n =
  let rec aux n acc =
    match n with
    | 0 ->
        acc
    | n ->
        aux (n - 1)
          ( match next ?create t with
          | Some x ->
              x :: acc
          | None ->
              acc )
  in
  aux n [] |> List.rev |> Array.of_list

type output =
  { filename: string
  ; pid: int
  ; pipe: Unix.file_descr
  ; width: int
  ; height: int }

let create ?(stdout = Unix.stdout) ?(stderr = Unix.stderr) ?(framerate = 30) filename width height =
  let (ic, oc) = Unix.pipe () in
  let pid = Unix.create_process "ffmpeg" [|
    "ffmpeg";
    "-y";
    "-f";
    "rawvideo";
    "-pixel_format";
    "rgb24";
    "-video_size";
    Printf.sprintf "%dx%d" width height;
    "-framerate";
    string_of_int framerate;
    "-i";
    "-";
    filename
  |] ic stdout stderr in
  {filename; pid; width; height; pipe = oc}

let finish output =
  Unix.close output.pipe

let kill output =
  Unix.kill Sys.sigint output.pid

external write_u8_bigarray: Unix.file_descr -> (int, u8) Data.t -> unit = "write_u8_bigarray"

let write_frame output image =
  write_u8_bigarray output.pipe (Image.data image)

