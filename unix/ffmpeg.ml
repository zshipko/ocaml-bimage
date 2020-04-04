open Bimage

module Input = struct
  type t =
    { filename: string
    ; pipe: in_channel
    ; width: int
    ; height: int
    ; num_frames: int
    ; mutable current_frame: int
    ; frame: (int, u8, rgb) Image.t}

  let get_num_frames filename =
    let cmd =
      "ffmpeg -i \"" ^ filename ^ "\" -map 0:v:0 -c copy -f null -y /dev/null 2>&1 | grep frame= | tail -n 1 | sed -n -e 's/^.*frame=//p' | awk '{ print $1 }'"
    in
    let proc = Unix.open_process_in cmd in
    let frames = input_line proc in
    close_in proc; int_of_string frames

  let get_size filename =
    let cmd =
      "ffprobe -v error -select_streams v:0 -show_entries stream=width,height \
       -of csv=s=x:p=0 \""
      ^ filename ^ "\""
    in
    let proc = Unix.open_process_in cmd in
    let size = input_line proc in
    let shape = String.split_on_char 'x' size in
    match shape with
    | x :: y :: _ ->
      (int_of_string x, int_of_string y)
    | _ ->
      Error.exc `Invalid_shape

  let create ?size filename  =
    let width, height = match size with
      | Some x -> x
      | None -> get_size filename
    in
    let ic = Unix.open_process_in (String.concat " " [
        "ffmpeg";
        "-v"; "error";
        "-i";
        "\"" ^ filename ^ "\"";
        "-pixel_format";
        "rgb24";
        "-video_size";
        Printf.sprintf "%dx%d" width height;
        "-f";
        "rawvideo";
        "-";
      ]) in
    {filename; width; height; pipe = ic;
     num_frames = get_num_frames filename; current_frame = 0;
     frame = Image.create u8 rgb width height}

  let close input =
    close_in input.pipe

  let read input =
    input.current_frame <- input.current_frame + 1;
    if input.current_frame >= input.num_frames then
      None
    else
      let data = Image.data input.frame in
      for i = 0 to Data.length data - 1 do
        data.{i} <- input_byte input.pipe
      done;
      Some input.frame
end

module Output = struct
  type t =
    { filename: string
    ; pipe: out_channel
    ; width: int
    ; height: int }

  let create ?(framerate = 30) filename (width, height) =
    let oc = Unix.open_process_out (String.concat " " [
        "ffmpeg";
        "-v"; "error";
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
        "\"" ^ filename ^ "\""
      ]) in
    {filename; width; height; pipe = oc}

  let close output =
    close_out output.pipe

  let write output image =
    let data = Image.data image in
    for i = 0 to Data.length data - 1 do
      output_byte output.pipe data.{i}
    done;
    flush output.pipe

end
