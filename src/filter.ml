type ('a, 'b, 'c) t = output:('a, 'b, 'c) Image.t -> Input.t -> unit

let v ?(x = ref 0) ?(y = ref 0) expr : ('a, 'b, 'c) t =
  let op = Expr.prepare x y expr in
  fun ~output inputs ->
    let width, height, _channels = Image.shape output in
    let rec inner () =
      if !y >= height then ()
      else
        let y' = !y in
        let () =
          for x' = 0 to width - 1 do
            x := x';
            let px = op inputs |> Pixel.of_rgb output.color in
            Image.set_pixel output x' y' px
          done
        in
        let () = incr y in
        inner ()
    in
    inner ()

let join (filters : Expr.pixel Expr.t list) : ('a, 'b, 'c) t =
  let filters = Array.of_list filters in
  fun ~output (inputs : Input.t) ->
    let rec inner i =
      if i >= Array.length filters then ()
      else
        let () = if i > 0 then inputs.(0) <- Image.any (Image.copy output) in
        let () = v filters.(i) ~output inputs in
        inner (i + 1)
    in
    inner 0

let run ~output inputs t =
  t ~output inputs;
  output

let run_expr ~output inputs x = run ~output inputs (v x)

let eval t ty color ?width ?height inputs =
  let (Image.Any first) = Input.get inputs 0 in
  let w, h, _ = Image.shape first in
  let width = match width with Some x -> x | None -> w in
  let height = match height with Some x -> x | None -> h in
  let output = Image.v ty color width height in
  run ~output inputs t

let eval_expr expr ty color ?width ?height inputs =
  eval (v expr) ty color ?width ?height inputs
