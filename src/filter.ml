module type FILTER = sig
  type 'a io

  type ('a, 'b, 'c) t = output:('a, 'b, 'c) Image.t -> Input.t -> unit io

  val join : ('a, 'b, 'c) t array -> ('a, 'b, 'c) t

  val make : ?x:int ref -> ?y:int ref -> Op.t -> ('a, 'b, 'c) t

  val of_expr : Expr.pixel Expr.t -> ('a, 'b, 'c) t
end

module Make (S : sig
  type 'a io

  val bind : 'a io -> ('a -> 'b io) -> 'b io

  val return : 'a -> 'a io

  val detach : ('a -> 'b) -> 'a -> 'b io
end) : FILTER with type 'a io = 'a S.io = struct
  type 'a io = 'a S.io

  type ('a, 'b, 'c) t = output:('a, 'b, 'c) Image.t -> Input.t -> unit io

  let join filters : ('a, 'b, 'c) t =
   fun ~output inputs ->
     let rec inner i =
       if i >= Array.length filters then S.return ()
       else
         let () = if i > 0 then inputs.(0) <- Input.input (Image.copy output) in
         S.bind (filters.(i) ~output inputs) (fun () -> inner (i + 1))
     in
     inner 0

  let make ?(x = ref 0) ?(y = ref 0) op :
      output:('a, 'b, 'c) Image.t -> Input.t -> unit io =
   fun ~output inputs ->
     let width, height, _channels = Image.shape output in
     let op = op inputs in
     let rec inner () =
       if !y >= height then S.return ()
       else
         S.bind
           (S.detach
              (fun y ->
                for x' = 0 to width - 1 do
                  x := x';
                  let px = op x' y in
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

  let of_expr (expr : Expr.pixel Expr.t) :
      output:('a, 'b, 'c) Image.t -> Input.t -> unit io =
    make (Expr.op expr)
end

include Make (struct
  type 'a io = 'a

  let detach f x = f x

  let return a = a

  let bind a f = f a
end)
