module type FILTER = sig
  type 'a io

  type ('a, 'b, 'c) t = output:('a, 'b, 'c) Image.t -> Input.t -> unit io

  val join : Expr.pixel Expr.t list -> ('a, 'b, 'c) t

  val v : ?x:int ref -> ?y:int ref -> Expr.pixel Expr.t -> ('a, 'b, 'c) t

  val run :
    output:('a, 'b, 'c) Image.t ->
    Input.t ->
    ('a, 'b, 'c) t ->
    ('a, 'b, 'c) Image.t

  val run_expr :
    output:('a, 'b, 'c) Image.t ->
    Input.t ->
    Expr.pixel Expr.t ->
    ('a, 'b, 'c) Image.t

  val eval :
    ('a, 'b, 'c) t ->
    ('a, 'b) Type.t ->
    'c Color.t ->
    ?width:int ->
    ?height:int ->
    Input.t ->
    ('a, 'b, 'c) Image.t

  val eval_expr :
    Expr.pixel Expr.t ->
    ('a, 'b) Type.t ->
    'c Color.t ->
    ?width:int ->
    ?height:int ->
    Input.t ->
    ('a, 'b, 'c) Image.t
end

module Make (S : sig
  type 'a io

  val bind : unit io -> (unit -> unit io) -> unit io

  val wrap : (unit -> 'a) -> 'a io

  val detach : ('a -> unit) -> 'a -> unit io

  val wait : unit io -> unit
end) : FILTER with type 'a io = 'a S.io = struct
  type 'a io = 'a S.io

  type ('a, 'b, 'c) t = output:('a, 'b, 'c) Image.t -> Input.t -> unit io

  let v ?(x = ref 0) ?(y = ref 0) expr : ('a, 'b, 'c) t =
    let op = Expr.compute_at expr in
    fun ~output inputs ->
      let width, height, _channels = Image.shape output in
      let rec inner () =
        if !y >= height then S.wrap (fun () -> ())
        else
          S.bind
            (S.detach
               (fun y ->
                 for x' = 0 to width - 1 do
                   x := x';
                   let px = op inputs x' y |> Pixel.of_rgb output.color in
                   Image.set_pixel output x' y px
                 done)
               !y)
            (fun () ->
              (* If x index is greater than the width then reset x index to 0
                 * and increment y index *)
              incr y;
              inner ())
      in
      inner ()

  let join (filters : Expr.pixel Expr.t list) : ('a, 'b, 'c) t =
    let filters = Array.of_list filters in
    fun ~output (inputs : Input.t) ->
      let rec inner i =
        if i >= Array.length filters then ()
        else
          let () = if i > 0 then inputs.(0) <- Image.any (Image.copy output) in
          let () = S.wait (v filters.(i) ~output inputs) in
          inner (i + 1)
      in
      S.wrap (fun () -> inner 0)

  let run ~output inputs t =
    S.wait (t ~output inputs);
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
end

include Make (struct
  type 'a io = 'a

  let detach f x = f x

  let wrap f = f ()

  let bind a f = f a

  let wait () = ()
end)
