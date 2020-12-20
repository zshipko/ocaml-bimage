module type FILTER = sig
  type 'a io

  type ('a, 'b, 'c) t =
    output:('a, 'b, 'c) Image.t -> Image.any array -> unit io

  val join : ('a, 'b, 'c) t list -> ('a, 'b, 'c) t

  val v : ?x:int ref -> ?y:int ref -> Expr.pixel Expr.t -> ('a, 'b, 'c) t

  val run :
    output:('a, 'b, 'c) Image.t ->
    Image.any array ->
    ('a, 'b, 'c) t ->
    ('a, 'b, 'c) Image.t

  val run_expr :
    Expr.pixel Expr.t ->
    ('a, 'b) Type.t ->
    'c Color.t ->
    ?width:int ->
    ?height:int ->
    Input.t ->
    ('a, 'b, 'c) Image.t
end

include FILTER with type 'a io = 'a

module Make (S : sig
  type 'a io

  val bind : unit io -> (unit -> unit io) -> unit io

  val detach : ('a -> unit) -> 'a -> unit io

  val wait : unit io -> unit

  val wrap : (unit -> 'a) -> 'a io
end) : FILTER with type 'a io = 'a S.io
