module type FILTER = sig
  type 'a io
  type ('a, 'b, 'c) t =
    output:('a, 'b, 'c) Image.t -> Input.t -> unit io

  val join: ('a, 'b, 'c) t array -> ('a, 'b, 'c) t
  val make:
    ?x: int ref ->
    ?y: int ref ->
    ?c: int ref ->
    Op.t -> ('a, 'b, 'c) t
  val of_expr:
    float Expr.t -> ('a, 'b, 'c) t
end

module Make(S: sig
  type 'a io

  val bind: 'a io -> ('a -> 'b io) -> 'b io
  val return: 'a -> 'a io
  val detach: ('a -> 'b) -> 'a -> 'b io
end) : FILTER with type 'a io = 'a S.io = struct
  type 'a io = 'a S.io

  type ('a, 'b, 'c) t =
    output:('a, 'b, 'c) Image.t -> Input.t -> unit io

  let join filters: ('a, 'b, 'c) t =
    fun ~output inputs ->
      let rec inner i =
        if i >= Array.length filters then
          S.return ()
        else
          let () = if i > 0 then
            inputs.(0) <- Input.input (Image.copy output) in
          S.bind (filters.(i) ~output inputs) (fun () -> inner (i + 1))
      in inner 0

  let make ?(x = ref 0) ?(y = ref 0) ?(c = ref 0) op : output:('a, 'b, 'c) Image.t -> Input.t -> unit io =
    fun ~output inputs ->
     let width, _height, channels = Image.shape output in
     let kind = Image.ty output in
     let of_float f = Type.of_float kind f in
     let denormalize = Type.denormalize kind in
     let clamp = Type.clamp kind in
     let op = op inputs in
     let rec inner i =
       if i >= Image.length output then S.return ()
       else
       S.bind (S.detach (fun (x, y, c) ->
         let f = op x y c in
         Bigarray.Array1.unsafe_set output.data i (of_float @@ denormalize @@ clamp f);
        ) (!x, !y, !c)) (fun () ->
       (* Increment channel index *)
       incr c;
       (* If channel index is greater than the number of channels
          * then reset channel index to 0 and increment x index *)
       let () =
         if !c >= channels then
           let () = c := 0 in
           incr x
       in
       (* If x index is greater than the width then reset x index to 0
          * and increment y index *)
       let () =
         if !x >= width then
           let () = x := 0 in
           incr y
       in inner (i + 1))
     in inner 0

  let of_expr expr = make (Expr.op expr)
end

include Make(struct
  type 'a io = 'a
  let detach f x = f x
  let return a = a
  let bind a f = f a
end)
