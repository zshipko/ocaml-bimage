val iter :
  (int -> int -> ('b, 'c) Bimage.Data.t -> 'a -> 'a) ->
  ?x:int ->
  ?y:int ->
  ?width:int ->
  ?height:int -> ('b, 'c, 'd) Bimage.Image.t -> 'a -> 'a Lwt.t


val eval :
 ?x:int ref ->
 ?y:int ref ->
 ?c:int ref ->
 (Bimage.Input.t -> int -> int -> int -> float) ->
 output:('a, 'b, 'c) Bimage.Image.t -> Bimage.Input.t -> unit Lwt.t
