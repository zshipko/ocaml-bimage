(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

let str = Format.asprintf
let pp = Format.fprintf
let err_unsupported_by = str "unsupported bigarray kind"
let err_not_nan = "not a NaN"
let err_empty_box = "empty box"
let err_packed_sf = "packed sample format"
let err_illegal_fourcc c = str "illegal FourCC code (%S)" c
let err_rresnone = "raster's res is None"
let err_rindex a v = str "index %s is %f but should be >= 1." a v
let err_rfirst v = str "first is %d but non-negative int expected" v
let err_rstride a v min = str "%s_stride is %d but should be >= %d" a v min
let err_rrange k a v min max =
  str "%s %s is %d but exepected in [%d;%d] range" k a v min max

let err_sample_pack p st =
  str "sample pack %s incompatible with scalar type %s" p st

let err_pp_ba_spec ~first ~stride ~count ~len =
  str "invalid bounds: first:%d + stride:%d * count:%d >= len:%d"
    first stride count len

let err_buffer_data data k =
  str "data argument %s is irrelevant for bigarray kind %s" data k

let err_buffer_kind =
  str "bigarray kind can't be represented by a Gg.Ba.scalar_type"

let pp_pad ppf len = for _i = 1 to len do Format.pp_print_space ppf () done
let pp_buf buf ppf fmt =
  let flush ppf =
    Format.pp_print_flush ppf ();
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s, String.length s
  in
  Format.kfprintf flush ppf fmt

let gg_eps = 1e-9

(* Floating point utilities. *)

module Float = struct
  type t = float

  (* See the .mli for a quick recall on OCaml's float representation. *)

  let bfloat_sign = 0x80_00_00_00_00_00_00_00L            (* sign  bit mask. *)
  let bfloat_exp  = 0x7F_F0_00_00_00_00_00_00L      (* biased exponent mask. *)
  let bfloat_frac = 0x00_0F_FF_FF_FF_FF_FF_FFL          (* significand mask. *)
  let bfloat_nanp = 0x00_07_FF_FF_FF_FF_FF_FFL          (* nan payload mask. *)
  let bfloat_qnan = 0x7F_F8_00_00_00_00_00_00L    (* a quiet nan, payload 0. *)

  (* Constants *)

  let e = 2.7182818284590452353602874713526625        (* values from math.h. *)
  let pi = 3.1415926535897932384626433832795029
  let two_pi = 2. *. pi
  let pi_div_2 = 1.5707963267948966192313216916397514
  let pi_div_4 = 0.7853981633974483096156608458198757
  let inv_pi = 0.3183098861837906715377675267450287

  let max_sub_float = Int64.float_of_bits 0x00_0F_FF_FF_FF_FF_FF_FFL
  let min_sub_float = Int64.float_of_bits 0x00_00_00_00_00_00_00_01L
  let max_frac_float = 4503599627370495.5                (* Float.pred 2^52. *)
  let max_int_arith = 9007199254740992.                             (* 2^53. *)

  (* Functions *)

  let r2d = 180. /. pi
  let d2r = pi /. 180.
  let deg_of_rad r =  r *. r2d
  let rad_of_deg d =  d *. d2r

  let pi2 = 2. *. pi
  let wrap_angle r =
    let r = mod_float (r +. pi) pi2 in
    if r < 0. then r +. pi else r -. pi

  let random ?(min = 0.) ~len () =
    let t0 = float (Random.bits ()) /. 1073741823. in (* ≠ from Random.float *)
    let t1 = (float (Random.bits ()) +. t0) /. 1073741824. in
    let t2 = (float (Random.bits ()) +. t1) /. 1073741824. in
    min +. (t2 *. len)

  let srandom s ?(min = 0.) ~len () =
    let t0 = float (Random.State.bits s) /. 1073741823. in     (* see above. *)
    let t1 = (float (Random.State.bits s) +. t0) /. 1073741824. in
    let t2 = (float (Random.State.bits s) +. t1) /. 1073741824. in
    min +. (t2 *. len)

  let mix x y t = x +. t *. (y -. x)
  let step : float -> float -> float = fun edge x -> if x < edge then 0. else 1.
  let smooth_step e0 e1 x =
    if x <= e0 then 0. else
    if x >= e1 then 1. else
      let t = (x -. e0) /. (e1 -. e0) in
      t *. t *. (3. -. 2. *. t)

  let fmax : float -> float -> float = fun x y ->
    if x <> x then (* x is NaN *) y else
    if x < y then y else (* x >= y or y = NaN *) x

  let fmin : float -> float -> float = fun x y ->
    if x <> x then (* x is NaN *) y else
    if y < x then y else (* x <= y or y = NaN *) x

  let clamp : min:float -> max:float -> float -> float = fun ~min ~max x ->
    if x < min then min else
    if x > max then max else x

  let remap ~x0 ~x1 ~y0 ~y1 v =
    if x0 = x1 then y0 else
      y0 +. ((v -. x0) /. (x1 -. x0)) *. (y1 -. y0)

  let round x = floor (x +. 0.5)
  let int_of_round x = truncate (round x)
  let round_dfrac d x =
    if x -. (round x) = 0. then x else                   (* x is an integer. *)
      let m = 10. ** (float d) in                       (* m moves 10^-d to 1. *)
      (floor ((x *. m) +. 0.5)) /. m

  let round_dsig d x =
    if x = 0. then 0. else
      let m = 10. ** (floor (log10 (abs_float x))) in       (* to normalize x. *)
      (round_dfrac d (x /. m)) *. m

  let round_zero ~eps x = if abs_float x < eps then 0. else x
  let chop ~eps x =
    if abs_float x > max_frac_float then x else
      let xi = floor (x +. 0.5) in
      if (abs_float (x -. xi)) < eps then xi else x

  let sign x = if x > 0. then 1. else (if x < 0. then -1. else x)
  let sign_bit x = (Int64.logand (Int64.bits_of_float x) bfloat_sign) <> 0L
  let succ x = match classify_float x with
    | FP_normal | FP_subnormal ->
      if x > 0. then Int64.float_of_bits (Int64.add (Int64.bits_of_float x) 1L)
      else Int64.float_of_bits (Int64.sub (Int64.bits_of_float x) 1L)
    | FP_zero -> min_sub_float
    | FP_infinite -> if x = neg_infinity then -. max_float else infinity
    | FP_nan -> x

  let pred x = match classify_float x with
    | FP_normal | FP_subnormal ->
      if x > 0. then Int64.float_of_bits (Int64.sub (Int64.bits_of_float x) 1L)
      else Int64.float_of_bits (Int64.add (Int64.bits_of_float x) 1L)
    | FP_zero -> -. min_sub_float
    | FP_infinite -> if x = infinity then max_float else neg_infinity
    | FP_nan -> x

  let nan p =
    let p = (Int64.logand (Int64.of_int p) bfloat_nanp) in
    Int64.float_of_bits (Int64.logor bfloat_qnan p)

  let nan_payload x =
    if x = x then invalid_arg err_not_nan else
      Int64.to_int (Int64.logand (Int64.bits_of_float x) bfloat_nanp)

  (* Predicates and comparisons *)

  let is_zero ~eps x = abs_float x < eps
  let is_nan : float -> bool = fun x -> x <> x
  let is_inf x = classify_float x = FP_infinite
  let is_int x = x -. (floor x) = 0.
  let equal : float -> float -> bool = ( = )
  let equal_tol ~eps x y =          (* NOTE code duplicate with compare_tol. *)
    if compare x y = 0 then true else
      let ax = abs_float x in
      let ay = abs_float y in
      let amax = if ax > ay then ax else ay in
      let max = if 1. > amax then 1. else amax in
      if max = infinity then false else
        abs_float (x -. y) <= eps *. max

  let compare : float -> float -> int = Pervasives.compare
  let compare_tol ~eps x y =          (* NOTE code duplicate with equal_tol. *)
    let c = compare x y in
    if c = 0 then 0 else
      let ax = abs_float x in
      let ay = abs_float y in
      let amax = if ax > ay then ax else ay in
      let max = if 1. > amax then 1. else amax in
      if max = infinity then c else
      if abs_float (x -. y) <= eps *. max then 0 else c

  (* Printers *)

  let pp ppf x =                                     (* too slow, ∃ better ? *)
    let pr_neg ppf neg = if neg then pp ppf "-" else () in
    match classify_float x with
    | FP_normal ->
      let x = Int64.bits_of_float x in
      let neg = Int64.logand x bfloat_sign <> 0L in
      let f = Int64.logand x bfloat_frac in
      let e =
        Int64.sub (Int64.shift_right (Int64.logand x bfloat_exp) 52) 1023L
      in
      pp ppf "%a0x1.%013LXp%Ld" pr_neg neg f e
    | FP_subnormal ->
      let f = Int64.logand (Int64.bits_of_float x) bfloat_frac in
      let neg = x < 0. in
      pp ppf "%a0x0.%013LXp-1022" pr_neg neg f
    | FP_zero ->
      let neg = Int64.logand (Int64.bits_of_float x) bfloat_sign <> 0L in
      pp ppf "%a0." pr_neg neg
    | FP_infinite ->
      let neg = x < 0. in
      pp ppf "%ainf" pr_neg neg
    | FP_nan ->
      let x = Int64.bits_of_float x in
      let neg = Int64.logand x bfloat_sign <> 0L in
      let p = Int64.logand x bfloat_nanp in
      pp ppf "%anan(0x%LX)" pr_neg neg p
end

(* Vector and matrix types are defined here so that they can be seen
   in every module. We use records of floats. This allows unboxed
   float storage and avoids the bound checks we'd get with arrays.

   The value [i] allows to (slowly) index the types like a linear array. *)

module V2t = struct
  type t = { x : float; y : float }
  let i = [| (fun a -> a.x); (fun a -> a.y); |]
end

module V3t = struct
  type t = { x : float; y : float; z : float }
  let i = [| (fun a -> a.x); (fun a -> a.y); (fun a -> a.z) |]
end

module V4t = struct
  type t = { x : float; y : float; z : float; w : float }
  let i = [| (fun a -> a.x); (fun a -> a.y); (fun a -> a.z); (fun a -> a.w) |]
end

module M2t = struct
  type t = { e00 : float; e10 : float; (* col 0 *)
             e01 : float; e11 : float; (* col 1 *) }
  let i = [| (fun a -> a.e00); (fun a -> a.e10);
             (fun a -> a.e01); (fun a -> a.e11); |]

  open V2t
  let row = [| (fun a -> { x = a.e00; y = a.e01 });
               (fun a -> { x = a.e10; y = a.e11 }) |]
  let col = [| (fun a -> { x = a.e00; y = a.e10 });
               (fun a -> { x = a.e01; y = a.e11 }) |]
end

module M3t = struct
  type t = { e00 : float; e10 : float; e20 : float; (* col 0 *)
             e01 : float; e11 : float; e21 : float; (* col 1 *)
             e02 : float; e12 : float; e22 : float; (* col 2 *) }
  let i = [| (fun a -> a.e00); (fun a -> a.e10); (fun a -> a.e20);
             (fun a -> a.e01); (fun a -> a.e11); (fun a -> a.e21);
             (fun a -> a.e02); (fun a -> a.e12); (fun a -> a.e22); |]

  open V3t
  let row = [| (fun a -> { x = a.e00; y = a.e01; z = a.e02});
               (fun a -> { x = a.e10; y = a.e11; z = a.e12});
               (fun a -> { x = a.e20; y = a.e21; z = a.e22}); |]
  let col = [| (fun a -> { x = a.e00; y = a.e10; z = a.e20});
               (fun a -> { x = a.e01; y = a.e11; z = a.e21});
               (fun a -> { x = a.e02; y = a.e12; z = a.e22}); |]
end

module M4t = struct
  type t = { e00 : float; e10 : float; e20 : float; e30 : float; (* col 0 *)
             e01 : float; e11 : float; e21 : float; e31 : float; (* col 1 *)
             e02 : float; e12 : float; e22 : float; e32 : float; (* col 2 *)
             e03 : float; e13 : float; e23 : float; e33 : float; (* col 3 *) }
  let i =
    [| (fun a -> a.e00); (fun a -> a.e10); (fun a -> a.e20); (fun a -> a.e30);
       (fun a -> a.e01); (fun a -> a.e11); (fun a -> a.e21); (fun a -> a.e31);
       (fun a -> a.e02); (fun a -> a.e12); (fun a -> a.e22); (fun a -> a.e32);
       (fun a -> a.e03); (fun a -> a.e13); (fun a -> a.e23); (fun a -> a.e33);|]

  open V4t
  let row = [| (fun a -> { x = a.e00; y = a.e01; z = a.e02; w = a.e03});
               (fun a -> { x = a.e10; y = a.e11; z = a.e12; w = a.e13});
               (fun a -> { x = a.e20; y = a.e21; z = a.e22; w = a.e23});
               (fun a -> { x = a.e30; y = a.e31; z = a.e32; w = a.e33}); |]
  let col = [| (fun a -> { x = a.e00; y = a.e10; z = a.e20; w = a.e30});
               (fun a -> { x = a.e01; y = a.e11; z = a.e21; w = a.e31});
               (fun a -> { x = a.e02; y = a.e12; z = a.e22; w = a.e32});
               (fun a -> { x = a.e03; y = a.e13; z = a.e23; w = a.e33}); |]
end

type m2 = M2t.t
type m3 = M3t.t
type m4 = M4t.t

(* Vectors *)

type v2 = V2t.t
type v3 = V3t.t
type v4 = V4t.t

module type V = sig
  type t
  val dim : int
  type m

  (* Constructors, accessors and constants *)

  val comp : int -> t -> float
  val zero : t
  val infinity : t
  val neg_infinity : t
  val basis : int -> t

  (* Functions *)

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val smul : float -> t -> t
  val half : t -> t
  val dot : t -> t -> float
  val norm : t -> float
  val norm2 : t -> float
  val unit : t -> t
  val homogene : t -> t
  val mix : t -> t -> float -> t
  val ltr : m -> t -> t

  (* Overridden Pervasives operators. *)

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : float -> t -> t
  val ( / ) : t -> float -> t

  (* Traversal *)

  val map : (float -> float) -> t -> t
  val mapi : (int -> float -> float) -> t -> t
  val fold : ('a -> float -> 'a) -> 'a -> t -> 'a
  val foldi : ('a -> int -> float -> 'a) -> 'a -> t -> 'a
  val iter : (float -> unit) -> t -> unit
  val iteri : (int -> float -> unit) ->  t -> unit

  (* Predicates and comparisons *)

  val for_all : (float -> bool) -> t -> bool
  val exists : (float -> bool) -> t -> bool
  val equal : t -> t -> bool
  val equal_f : (float -> float -> bool) -> t -> t -> bool
  val compare : t -> t -> int
  val compare_f : (float -> float -> int) -> t -> t -> int

  (* Printers *)

  val pp : Format.formatter -> t -> unit
  val pp_f : (Format.formatter -> float -> unit) -> Format.formatter ->
    t -> unit
end

module V2 = struct
  open V2t
  type t = v2
  type m = m2
  let dim = 2

  (* Constructors, accessors and constants *)

  let v x y = { x = x; y = y }
  let comp i = V2t.i.(i)
  let x a = a.x
  let y a = a.y
  let ox = v 1. 0.
  let oy = v 0. 1.
  let zero = v 0. 0.
  let infinity = v infinity infinity
  let neg_infinity = v neg_infinity neg_infinity
  let _basis = [| ox; oy |]
  let basis i = _basis.(i)
  let of_tuple (x, y) = v x y
  let to_tuple a = (a.x, a.y)
  let of_polar pv = v (pv.x *. (cos pv.y)) (pv.x *. (sin pv.y))
  let to_polar a = v (sqrt (a.x *. a.x +. a.y *. a.y)) (atan2 a.y a.x)
  let of_v3 a = v a.V3t.x a.V3t.y
  let of_v4 a = v a.V4t.x a.V4t.y

  (* Functions *)

  let neg a = v (-. a.x) (-. a.y)
  let add a b = v (a.x +. b.x) (a.y +. b.y)
  let sub a b = v (a.x -. b.x) (a.y -. b.y)
  let mul a b = v (a.x *. b.x) (a.y *. b.y)
  let div a b = v (a.x /. b.x) (a.y /. b.y)
  let smul s a = v (s *. a.x) (s *. a.y)
  let half a = smul 0.5 a
  let dot a b = a.x *. b.x +. a.y *. b.y
  let norm a = hypot a.x a.y
  let norm2 a = a.x *. a.x +. a.y *. a.y
  let unit a = smul (1.0 /. (norm a)) a
  let polar r theta = v (r *. (cos theta)) ( r *. (sin theta))
  let angle a = atan2 a.y a.x
  let homogene a = if a.y <> 0. then v (a.x /. a.y) 1.0 else a
  let ortho a = v (-. a.y) a.x
  let mix a b t = v (a.x +. t *. (b.x -. a.x)) (a.y +. t *. (b.y -. a.y))
  let ltr m a =
    let open M2t in
    v (m.e00 *. a.x +. m.e01 *. a.y) (m.e10 *. a.x +. m.e11 *. a.y)

  let tr m a =
    let open M3t in
    v (m.e00 *. a.x +. m.e01 *. a.y) (m.e10 *. a.x +. m.e11 *. a.y)

  (* Overridden Pervasives operators. *)

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = smul
  let ( / ) v t = smul (1. /. t) v

  (* Traversal *)

  let map f a = v (f a.x) (f a.y)
  let mapi f a = v (f 0 a.x) (f 1 a.y)
  let fold f acc a = f (f acc a.x) a.y
  let foldi f acc a = f (f acc 0 a.x) 1 a.y
  let iter f a = f a.x; f a.y
  let iteri f a = f 0 a.x; f 1 a.y

  (* Predicates and comparisons *)

  let for_all p a = p a.x && p a.y
  let exists p a = p a.x || p a.y
  let equal = ( = )
  let equal_f eq a b = (eq a.x b.x) && (eq a.y b.y)
  let compare = Pervasives.compare
  let compare_f cmp a b =
    let c = cmp a.x b.x in if c <> 0 then c else
      let c = cmp a.y b.y in c

  (* Printers *)

  let pp ppf a = pp ppf "@[<1>(%g@ %g)@]" a.x a.y
  let pp_f pp_c ppf a = Format.fprintf ppf "@[<1>(%a@ %a)@]" pp_c a.x pp_c a.y
end

module V3 = struct
  open V3t
  type t = v3
  type m = m3
  let dim = 3

  (* Constructors, accessors and constants *)

  let v x y z = { x = x; y = y; z = z }
  let comp i = V3t.i.(i)
  let x a = a.x
  let y a = a.y
  let z a = a.z
  let ox = v 1. 0. 0.
  let oy = v 0. 1. 0.
  let oz = v 0. 0. 1.
  let zero = v 0. 0. 0.
  let infinity = v infinity infinity infinity
  let neg_infinity = v neg_infinity neg_infinity neg_infinity
  let _basis = [| ox; oy; oz |]
  let basis i = _basis.(i)
  let of_tuple (x, y, z) = v x y z
  let to_tuple a = (a.x, a.y, a.z)

  let of_spherical sv =
    let tc = cos sv.y in let ts = sin sv.y in
    let pc = cos sv.z in let ps = sin sv.z in
    v (sv.x *. tc *. ps) (sv.x *. ts *. ps) (sv.x *. pc)

  let to_spherical a =
    let r = sqrt (a.x *. a.x +. a.y *. a.y +. a.z *. a.z) in
    v r (atan2 a.y a.x) (acos (a.z /. r))

  let of_v2 a ~z = v a.V2t.x a.V2t.y z
  let of_v4 a = v a.V4t.x a.V4t.y a.V4t.z

  (* Functions *)

  let neg a = v (-. a.x) (-. a.y) (-. a.z)
  let add a b = v (a.x +. b.x) (a.y +. b.y) (a.z +. b.z)
  let sub a b = v (a.x -. b.x) (a.y -. b.y) (a.z -. b.z)
  let mul a b = v (a.x *. b.x) (a.y *. b.y) (a.z *. b.z)
  let div a b = v (a.x /. b.x) (a.y /. b.y) (a.z /. b.z)
  let smul s a = v (s *. a.x) (s *. a.y) (s *. a.z)
  let half a = smul 0.5 a
  let cross a b = v
      ((a.y *. b.z) -. (a.z *. b.y))
      ((a.z *. b.x) -. (a.x *. b.z))
      ((a.x *. b.y) -. (a.y *. b.x))

  let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z
  let norm a = (* avoid {under,over}flows *)
    let x = abs_float a.x in
    let y = abs_float a.y in
    let z = abs_float a.z in
    let x, y, z =
      if x >= y && x >= z then x, y, z else
      if y >= z           then y, x, z else
        z, x, y
    in
    if x = 0. then 0. else
      let y = y /. x in
      let z = z /. x in
      x *. sqrt (1. +. y *. y +. z *. z)

  let norm2 a = a.x *. a.x +. a.y *. a.y +. a.z *. a.z
  let unit a = smul (1. /. (norm a)) a
  let spherical r theta phi =
    let tc = cos theta in let ts = sin theta in
    let pc = cos phi in let ps = sin phi in
    v (r *. tc *. ps) (r *. ts *. ps) (r *. pc)

  let azimuth a = atan2 a.y a.x
  let zenith a =
    let r = sqrt (a.x *. a.x +. a.y *. a.y +. a.z *. a.z) in
    acos (a.z /. r)

  let homogene a = if a.z <> 0. then v (a.x /. a.z) (a.y /. a.z) 1.0 else a
  let mix a b t = v
      (a.x +. t *. (b.x -. a.x))
      (a.y +. t *. (b.y -. a.y))
      (a.z +. t *. (b.z -. a.z))

  let ltr m a =
    let open M3t in
    v (m.e00 *. a.x +. m.e01 *. a.y +. m.e02 *. a.z)
      (m.e10 *. a.x +. m.e11 *. a.y +. m.e12 *. a.z)
      (m.e20 *. a.x +. m.e21 *. a.y +. m.e22 *. a.z)

  let tr m a =
    let open M4t in
    v (m.e00 *. a.x +. m.e01 *. a.y +. m.e02 *. a.z)
      (m.e10 *. a.x +. m.e11 *. a.y +. m.e12 *. a.z)
      (m.e20 *. a.x +. m.e21 *. a.y +. m.e22 *. a.z)

  (* Overridden Pervasives operators. *)

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = smul
  let ( / ) v t = smul (1. /. t) v

  (* Traversal *)

  let map f a = v (f a.x) (f a.y) (f a.z)
  let mapi f a = v (f 0 a.x) (f 1 a.y) (f 2 a.z)
  let fold f acc a = f (f (f acc a.x) a.y) a.z
  let foldi f acc a = f (f (f acc 0 a.x) 1 a.y) 2 a.z
  let iter f a = f a.x; f a.y; f a.z
  let iteri f a = f 0 a.x; f 1 a.y; f 2 a.z

  (* Predicates and comparisons *)

  let for_all p a = p a.x && p a.y && p a.z
  let exists p a = p a.x || p a.y || p a.z
  let equal = ( = )
  let equal_f eq a b = (eq a.x b.x) && (eq a.y b.y) && (eq a.z b.z)
  let compare = Pervasives.compare
  let compare_f cmp a b =
    let c = cmp a.x b.x in if c <> 0 then c else
      let c = cmp a.y b.y in if c <> 0 then c else
        let c = cmp a.z b.z in c

  (* Printers *)

  let pp ppf a = pp ppf "@[<1>(%g@ %g@ %g)@]" a.x a.y a.z
  let pp_f pp_c ppf a = Format.fprintf ppf "@[<1>(%a@ %a@ %a)@]"
      pp_c a.x pp_c a.y pp_c a.z
end

module V4 = struct
  open V4t
  type t = v4
  type m = m4
  let dim = 4

  (* Constructors, accessors and constants *)

  let v x y z w = { x = x; y = y; z = z; w = w }
  let comp i = V4t.i.(i)
  let x a = a.x
  let y a = a.y
  let z a = a.z
  let w a = a.w
  let ox = v 1. 0. 0. 0.
  let oy = v 0. 1. 0. 0.
  let oz = v 0. 0. 1. 0.
  let ow = v 0. 0. 0. 1.
  let zero = v 0. 0. 0. 0.
  let infinity = v infinity infinity infinity infinity
  let neg_infinity = v neg_infinity neg_infinity neg_infinity neg_infinity
  let _basis = [| ox; oy; oz; ow |]
  let basis i = _basis.(i)
  let of_tuple (x, y, z, w) = v x y z w
  let to_tuple a = (a.x, a.y, a.z, a.w)
  let of_v2 a ~z ~w = v a.V2t.x a.V2t.y z w
  let of_v3 a ~w = v a.V3t.x a.V3t.y a.V3t.z w

  (* Functions *)

  let neg a = v (-. a.x) (-. a.y) (-. a.z) (-. a.w)
  let add a b = v (a.x +. b.x) (a.y +. b.y) (a.z +. b.z) (a.w +. b.w)
  let sub a b = v (a.x -. b.x) (a.y -. b.y) (a.z -. b.z) (a.w -. b.w)
  let mul a b = v (a.x *. b.x) (a.y *. b.y) (a.z *. b.z) (a.w *. b.w)
  let div a b = v (a.x /. b.x) (a.y /. b.y) (a.z /. b.z) (a.w /. b.w)
  let smul s a = v (s *. a.x) (s *. a.y) (s *. a.z) (s *. a.w)
  let half a = smul 0.5 a
  let dot a b = (a.x *. b.x) +. (a.y *. b.y) +. (a.z *. b.z) +. (a.w *. b.w)
  let norm a = (* avoid {under,over}flows *)
    let x = abs_float a.x in
    let y = abs_float a.y in
    let z = abs_float a.z in
    let w = abs_float a.w in
    let x, y, z, w =
      if x >= y && x >= z && x >= w then x, y, z, w else
      if y >= z && y >=  w          then y, x, z, w else
      if z >= w                     then z, x, y, w else
        w, x, y, z
    in
    if x = 0. then 0. else
      let y = y /. x in
      let z = z /. x in
      let w = w /. x in
      x *. sqrt (1. +. y *. y +. z *. z +. w *. w)

  let norm2 a = a.x *. a.x +. a.y *. a.y +. a.z *. a.z +. a.w *. a.w
  let unit a = smul (1. /. (norm a)) a
  let homogene a =
    if a.w <> 0. then v (a.x /. a.w) (a.y /. a.w) (a.z /. a.w) 1.0 else a

  let mix a b t = v
      (a.x +. t *. (b.x -. a.x))
      (a.y +. t *. (b.y -. a.y))
      (a.z +. t *. (b.z -. a.z))
      (a.w +. t *. (b.w -. a.w))

  let ltr m a =
    let open M4t in
    v (m.e00 *. a.x +. m.e01 *. a.y +. m.e02 *. a.z +. m.e03 *. a.w)
      (m.e10 *. a.x +. m.e11 *. a.y +. m.e12 *. a.z +. m.e13 *. a.w)
      (m.e20 *. a.x +. m.e21 *. a.y +. m.e22 *. a.z +. m.e23 *. a.w)
      (m.e30 *. a.x +. m.e31 *. a.y +. m.e32 *. a.z +. m.e33 *. a.w)

  (* Overridden Pervasives operators. *)

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = smul
  let ( / ) v t = smul (1. /. t) v

  (* Traversal *)

  let map f a = v (f a.x) (f a.y) (f a.z) (f a.w)
  let mapi f a = v (f 0 a.x) (f 1 a.y) (f 2 a.z) (f 3 a.w)
  let fold f acc a = f (f (f (f acc a.x) a.y) a.z) a.w
  let foldi f acc a = f (f (f (f acc 0 a.x) 1 a.y) 2 a.z) 3 a.w
  let iter f a = f a.x; f a.y; f a.z; f a.w
  let iteri f a = f 0 a.x; f 1 a.y; f 2 a.z; f 3 a.w

  (* Predicates and comparisons *)

  let for_all p a = p a.x && p a.y && p a.z && p a.w
  let exists p a = p a.x || p a.y || p a.z || p a.w
  let equal = ( = )
  let equal_f eq a b =
    (eq a.x b.x) && (eq a.y b.y) && (eq a.z b.z) && (eq a.w b.w)

  let compare = Pervasives.compare
  let compare_f cmp a b =
    let c = cmp a.x b.x in if c <> 0 then c else
      let c = cmp a.y b.y in if c <> 0 then c else
        let c = cmp a.z b.z in if c <> 0 then c else
          let c = cmp a.w b.w in c

  (* Printers *)

  let pp ppf a = pp ppf "@[<1>(%g@ %g@ %g@ %g)@]" a.x a.y a.z a.w
  let pp_f pp_c ppf a = Format.fprintf ppf "@[<1>(%a@ %a@ %a@ %a)@]"
      pp_c a.x pp_c a.y pp_c a.z pp_c a.w
end

(* Points *)

type p2 = v2
type p3 = v3

module type P = sig
  type t
  val dim : int
  type mh

  (* Constructors, accessors and constants *)

  val o : t

  (* Functions *)

  val mid : t -> t -> t
  val tr : mh -> t -> t
end

module P2 = struct
  open V2t
  type t = p2
  let dim = 2
  type mh = m3

  (* Constructors, accessors and constants *)

  let v = V2.v
  let x = V2.x
  let y = V2.y
  let o = V2.zero

  (* Functions *)

  let mid p q = v (0.5 *. (p.x +. q.x)) (0.5 *. (p.y +. q.y))

  let tr m p =
    let open M3t in
    v (m.e00 *. p.x +. m.e01 *. p.y +. m.e02)
      (m.e10 *. p.x +. m.e11 *. p.y +. m.e12)
end

module P3 = struct
  open V3t
  type t = p3
  let dim = 3
  type mh = m4

  (* Constructors, accessors and constants *)

  let v = V3.v
  let x = V3.x
  let y = V3.y
  let z = V3.z
  let o = V3.zero

  (* Functions *)

  let mid p q =
    v (0.5 *. (p.x +. q.x))
      (0.5 *. (p.y +. q.y))
      (0.5 *. (p.z +. q.z))

  let tr m p =
    let open M4t in
    v (m.e00 *. p.x +. m.e01 *. p.y +. m.e02 *. p.z +. m.e03)
      (m.e10 *. p.x +. m.e11 *. p.y +. m.e12 *. p.z +. m.e13)
      (m.e20 *. p.x +. m.e21 *. p.y +. m.e22 *. p.z +. m.e23)
end

(* Quaternions *)

type quat = v4

module Quat = struct
  open V4t
  type t = quat

  (* Constructors, accessors and constants *)

  let v = V4.v
  let zero = V4.zero
  let id = V4.ow

  (* Functions *)

  let mul q r =
    v (q.y *. r.z -. q.z *. r.y +. q.x *. r.w +. q.w *. r.x)
      (q.z *. r.x -. q.x *. r.z +. q.y *. r.w +. q.w *. r.y)
      (q.x *. r.y -. q.y *. r.x +. q.z *. r.w +. q.w *. r.z)
      (q.w *. r.w -. q.x *. r.x -. q.y *. r.y -. q.z *. r.z)

  let conj q = v (-.q.x) (-.q.y) (-.q.z) q.w
  let unit = V4.unit
  let inv q =
    let m = V4.norm2 q in
    V4.smul (1.0 /. m) (conj q)

  let slerp q r t =
    let cosv = V4.dot q r in
    let a = acos cosv in
    if a < gg_eps then q else
      let sinv = sin a in
      let c = (sin ((1.0 -. t) *. a)) /. sinv in
      let c' = (sin (t *. a)) /. sinv in
      V4.add (V4.smul c q) (V4.smul c' r)

  let squad q cq cr r t =
    let u = slerp q r t in
    let v = slerp cq cr t in
    slerp u v (2.0 *. t *. (1.0 -. t))

  let nlerp q r t = V4.unit (V4.add q (V4.smul t (V4.sub r q)))

  (* 3D space transformations} *)

  let of_m3 m =                           (* NOTE code duplicate with of_m4. *)
    let open M3t in
    let v x y z w = unit (v x y z w) in
    let tr = 1. +. m.e00 +. m.e11 +. m.e22 in
    if (tr > 0.0) then
      let s = (sqrt tr) *. 2. in
      v ((m.e21 -. m.e12) /. s)
        ((m.e02 -. m.e20) /. s)
        ((m.e10 -. m.e01) /. s)
        (0.25 *. s)
    else if (m.e00 > m.e11 && m.e00 > m.e22) then
      let s = sqrt (1. +. m.e00 -. m.e11 -. m.e22) *. 2. in
      v (0.25 *. s)
        ((m.e10 +. m.e01) /. s)
        ((m.e02 +. m.e20) /. s)
        ((m.e21 -. m.e12) /. s)
    else if (m.e11 > m.e22) then
      let s = sqrt (1. +. m.e11 -. m.e00 -. m.e22) *. 2. in
      v ((m.e10 +. m.e01) /. s)
        (0.25 *. s)
        ((m.e21 +. m.e12) /. s)
        ((m.e02 -. m.e20) /. s)
    else
      let s = sqrt (1. +. m.e22 -. m.e00 -. m.e11) *. 2. in
      v ((m.e02 +. m.e20) /. s)
        ((m.e21 +. m.e12) /. s)
        (0.25 *. s)
        ((m.e10 -. m.e01) /. s)

  let of_m4 m =                           (* NOTE code duplicate with of_m3. *)
    let open M4t in
    let v x y z w = unit (v x y z w) in
    let tr = 1. +. m.e00 +. m.e11 +. m.e22 in
    if (tr > 0.0) then
      let s = (sqrt tr) *. 2. in
      v ((m.e21 -. m.e12) /. s)
        ((m.e02 -. m.e20) /. s)
        ((m.e10 -. m.e01) /. s)
        (0.25 *. s)
    else if (m.e00 > m.e11 && m.e00 > m.e22) then
      let s = sqrt (1. +. m.e00 -. m.e11 -. m.e22) *. 2. in
      v (0.25 *. s)
        ((m.e10 +. m.e01) /. s)
        ((m.e02 +. m.e20) /. s)
        ((m.e21 -. m.e12) /. s)
    else if (m.e11 > m.e22) then
      let s = sqrt (1. +. m.e11 -. m.e00 -. m.e22) *. 2. in
      v ((m.e10 +. m.e01) /. s)
        (0.25 *. s)
        ((m.e21 +. m.e12) /. s)
        ((m.e02 -. m.e20) /. s)
    else
      let s = sqrt (1. +. m.e22 -. m.e00 -. m.e11) *. 2. in
      v ((m.e02 +. m.e20) /. s)
        ((m.e21 +. m.e12) /. s)
        (0.25 *. s)
        ((m.e10 -. m.e01) /. s)

  let rot3_map u u' =
    let e = V3.dot u u' in
    let c = V3.cross u u' in
    let r = sqrt (2. *. (1. +. e)) in
    v (c.V3t.x /. r) (c.V3t.y /. r) (c.V3t.z /. r) (r /. 2.)

  let rot3_axis u theta =
    let a = theta *. 0.5 in
    let s = sin a in
    v (s *. u.V3t.x) (s *. u.V3t.y) (s *. u.V3t.z) (cos a)

  let rot3_zyx r =
    let hz = V3.z r *. 0.5 in
    let hy = V3.y r *. 0.5 in
    let hx = V3.x r *. 0.5 in
    let cz = cos hz in let sz = sin hz in
    let cy = cos hy in let sy = sin hy in
    let cx = cos hx in let sx = sin hx in
    let cycz = cy *. cz in let sysz = sy *. sz in
    let cysz = cy *. sz in let sycz = sy *. cz in
    v (cycz *. sx -. sysz *. cx)
      (cysz *. sx +. sycz *. cx)
      (cysz *. cx -. sycz *. sx)
      (cycz *. cx +. sysz *. sx)

  let to_rot3_axis q =
    let a_2 = acos q.w in
    if a_2 < gg_eps then (V3.v 1.0 0.0 0.0), 0.0  else
      let d = 1.0 /. (sin a_2) in
      (V3.v (q.x *. d) (q.y *. d) (q.z *. d)), (a_2 *. 2.0)

  let to_rot3_zyx q =
    let xx = q.x *. q.x in let yy = q.y *. q.y in let zz = q.z *. q.z in
    let ww = q.w *. q.w in
    let wx = q.w *. q.x in let wy = q.w *. q.y in let wz = q.w *. q.z in
    let zx = q.z *. q.x in let zy = q.z *. q.y in
    let xy = q.x *. q.y in V3.v
      (atan2 (2. *. (zy +. wx)) (ww -. xx -. yy +. zz))
      (asin (-2. *. (zx -. wy)))
      (atan2 (2. *. (xy +. wz)) (ww +. xx -. yy -. zz))

  let apply3 q v =                      (* NOTE, code duplicate with apply4. *)
    let wx = q.w *. q.x in let wy = q.w *. q.y in let wz = q.w *. q.z in
    let xx = q.x *. q.x in let xy = q.x *. q.y in let xz = q.x *. q.z in
    let yy = q.y *. q.y in let yz = q.y *. q.z in let zz = q.z *. q.z in
    let x = v.V3t.x in let y = v.V3t.y in let z = v.V3t.z in V3.v
      (x +. 2. *. ((-. yy -. zz) *. x +. (xy -. wz) *. y +. (wy +. xz) *. z))
      (y +. 2. *. ((wz +. xy) *. x +. (-. xx -. zz) *. y +. (yz -. wx) *. z))
      (z +. 2. *. ((xz -. wy) *. x +. (wx +. yz) *. y +. (-. xx -. yy) *. z))

  let apply4 q v =                      (* NOTE, code duplicate with apply3. *)
    let wx = q.w *. q.x in let wy = q.w *. q.y in let wz = q.w *. q.z in
    let xx = q.x *. q.x in let xy = q.x *. q.y in let xz = q.x *. q.z in
    let yy = q.y *. q.y in let yz = q.y *. q.z in let zz = q.z *. q.z in
    let x = v.x in let y = v.y in let z = v.z in V4.v
      (x +. 2. *. ((-. yy -. zz) *. x +. (xy -. wz) *. y +. (wy +. xz) *. z))
      (y +. 2. *. ((wz +. xy) *. x +. (-. xx -. zz) *. y +. (yz -. wx) *. z))
      (z +. 2. *. ((xz -. wy) *. x +. (wx +. yz) *. y +. (-. xx -. yy) *. z))
      v.w
end

(* Matrices *)

module type M = sig
  type t
  val dim : int
  type v

  (* Constructors, accessors and constants *)

  val el : int -> int -> t -> float
  val row : int -> t -> v
  val col : int -> t -> v
  val zero : t
  val id : t

  (* Functions *)

  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val emul : t -> t -> t
  val ediv : t -> t -> t
  val smul : float -> t -> t
  val transpose : t -> t
  val trace : t -> float
  val det : t -> float
  val inv : t -> t

  (* Traversal *)

  val map : (float -> float) -> t -> t
  val mapi : (int -> int -> float -> float) -> t -> t
  val fold : ('a -> float -> 'a) -> 'a -> t -> 'a
  val foldi : ('a -> int -> int -> float -> 'a) -> 'a -> t -> 'a
  val iter : (float -> unit) -> t -> unit
  val iteri : (int -> int -> float -> unit) ->  t -> unit

  (* Predicates and comparisons *)

  val for_all : (float -> bool) -> t -> bool
  val exists : (float -> bool) -> t -> bool
  val equal : t -> t -> bool
  val equal_f : (float -> float -> bool) -> t -> t -> bool
  val compare : t -> t -> int
  val compare_f : (float -> float -> int) -> t -> t -> int

  (* Printers *)

  val pp : Format.formatter -> t -> unit
  val pp_f : (Format.formatter -> float -> unit) -> Format.formatter ->
    t -> unit
end

module M2 = struct
  open M2t
  open V2t
  type t = m2
  let dim = 2
  type v = v2

  (* Constructors, accessors and constants *)

  let v e00 e01 e10 e11 = { e00 = e00; e10 = e10; e01 = e01; e11 = e11}
  let of_rows r0 r1 = v r0.x r0.y r1.x r1.y
  let of_cols c0 c1 = v c0.x c1.x c0.y c1.y
  let el row col = M2t.i.(dim * col + row)
  let e00 a = a.e00
  let e01 a = a.e01
  let e10 a = a.e10
  let e11 a = a.e11
  let row r = M2t.row.(r)
  let col c = M2t.col.(c)
  let zero = v 0. 0. 0. 0.
  let id = v 1. 0. 0. 1.
  let of_m3 a = v a.M3t.e00 a.M3t.e01 a.M3t.e10 a.M3t.e11
  let of_m4 a = v a.M4t.e00 a.M4t.e01 a.M4t.e10 a.M4t.e11

  (* Functions *)

  let neg a =
    v (-. a.e00) (-. a.e01)
      (-. a.e10) (-. a.e11)

  let add a b =
    v (a.e00 +. b.e00) (a.e01 +. b.e01)
      (a.e10 +. b.e10) (a.e11 +. b.e11)

  let sub a b =
    v (a.e00 -. b.e00) (a.e01 -. b.e01)
      (a.e10 -. b.e10) (a.e11 -. b.e11)

  let mul a b =
    if a == id then b else
    if b == id then a else
      v (a.e00 *. b.e00 +. a.e01 *. b.e10) (a.e00 *. b.e01 +. a.e01 *. b.e11)
        (a.e10 *. b.e00 +. a.e11 *. b.e10) (a.e10 *. b.e01 +. a.e11 *. b.e11)

  let emul a b =
    v (a.e00 *. b.e00) (a.e01 *. b.e01)
      (a.e10 *. b.e10) (a.e11 *. b.e11)

  let ediv a b =
    v (a.e00 /. b.e00) (a.e01 /. b.e01)
      (a.e10 /. b.e10) (a.e11 /. b.e11)

  let smul s a =
    v (s *. a.e00) (s *. a.e01)
      (s *. a.e10) (s *. a.e11)

  let transpose a =
    v a.e00 a.e10
      a.e01 a.e11

  let trace a = a.e00 +. a.e11
  let det a = a.e00 *. a.e11 -. a.e01 *. a.e10
  let inv a =
    let det = a.e00 *. a.e11 -. a.e01 *. a.e10 in
    v (   a.e11 /. det) (-. a.e01 /. det)
      (-. a.e10 /. det) (   a.e00 /. det)

  (* 2D space transformations *)

  let rot2 theta =
    let c = cos theta in
    let s = sin theta in
    v c (-. s)
      s c

  let scale2 s =
    v s.x 0.
      0.  s.y

  (* Traversal *)

  let map f a =
    v (f a.e00) (f a.e01)
      (f a.e10) (f a.e11)

  let mapi f a =
    v (f 0 0 a.e00) (f 0 1 a.e01)
      (f 1 0 a.e10) (f 1 1 a.e11)

  let fold f acc a =
    f (f (f (f acc a.e00) a.e10) a.e01) a.e11

  let foldi f acc a =
    f (f (f (f acc 0 0 a.e00) 1 0 a.e10) 0 1 a.e01) 1 1 a.e11

  let iter f a = f a.e00; f a.e10; f a.e01; f a.e11
  let iteri f a = f 0 0 a.e00; f 1 0 a.e10; f 0 1 a.e01; f 1 1 a.e11

  (* Predicates and comparisons *)

  let for_all p a = p a.e00 && p a.e10 && p a.e01 && p a.e11
  let exists p a = p a.e00 || p a.e10 || p a.e01 || p a.e11
  let equal = (=)
  let equal_f eq a b =
    eq a.e00 b.e00 && eq a.e10 b.e10 && eq a.e01 b.e01 && eq a.e11 b.e11

  let compare = Pervasives.compare
  let compare_f cmp a b =
    let c = cmp a.e00 b.e00 in if c <> 0 then c else
      let c = cmp a.e10 b.e10 in if c <> 0 then c else
        let c = cmp a.e01 b.e01 in if c <> 0 then c else
          let c = cmp a.e11 b.e11 in c

  (* Printers *)

  let pp_f pp_e ppf a =
    let max : int -> int -> int = fun a b -> if a > b then a else b in
    let b = Buffer.create 30 in
    let bppf = Format.formatter_of_buffer b in
    let e00, e00l = pp_buf b bppf "%a" pp_e a.e00 in
    let e10, e10l = pp_buf b bppf "%a" pp_e a.e10 in
    let max0 = max e00l e10l in
    let e01, e01l = pp_buf b bppf "%a" pp_e a.e01 in
    let e11, e11l = pp_buf b bppf "%a" pp_e a.e11 in
    let max1 = max e01l e11l in
    pp ppf
      "@[<v>@[<1>|%a%s@ %a%s|@]@,\
       @[<1>|%a%s@ %a%s|@]@]"
      pp_pad (max0 - e00l) e00 pp_pad (max1 - e01l) e01
      pp_pad (max0 - e10l) e10 pp_pad (max1 - e11l) e11

  let pp_e_default ppf = pp ppf "%g"
  let pp ppf a = pp_f pp_e_default ppf a
end

module M3 = struct
  open M3t
  open V3t
  type t = m3
  let dim = 3
  type v = v3

  (* Constructors, accessors and constants *)

  let v e00 e01 e02 e10 e11 e12 e20 e21 e22 =
    { e00 = e00; e10 = e10; e20 = e20;
      e01 = e01; e11 = e11; e21 = e21;
      e02 = e02; e12 = e12; e22 = e22; }

  let of_rows r0 r1 r2 = v r0.x r0.y r0.z r1.x r1.y r1.z r2.x r2.y r2.z
  let of_cols c0 c1 c2 = v c0.x c1.x c2.x c0.y c1.y c2.y c0.z c1.z c2.z
  let el row col = M3t.i.(dim * col + row)
  let e00 a = a.e00
  let e01 a = a.e01
  let e02 a = a.e02
  let e10 a = a.e10
  let e11 a = a.e11
  let e12 a = a.e12
  let e20 a = a.e20
  let e21 a = a.e21
  let e22 a = a.e22
  let row r = M3t.row.(r)
  let col c = M3t.col.(c)
  let zero = v 0. 0. 0. 0. 0. 0. 0. 0. 0.
  let id = v 1. 0. 0. 0. 1. 0. 0. 0. 1.
  let of_m2_v2 a u =
    v a.M2t.e00 a.M2t.e01 u.V2t.x
      a.M2t.e10 a.M2t.e11 u.V2t.y
      0.        0.        1.

  let of_m4 a =
    v a.M4t.e00 a.M4t.e01 a.M4t.e02
      a.M4t.e10 a.M4t.e11 a.M4t.e12
      a.M4t.e20 a.M4t.e21 a.M4t.e22

  let of_quat q =                   (* NOTE, code duplicate with M4.of_quat. *)
    let open V4t in
    let x2 = q.x +. q.x in let y2 = q.y +. q.y in let z2 = q.z +. q.z in
    let xx2 = x2 *. q.x in let xy2 = x2 *. q.y in let xz2 = x2 *. q.z in
    let xw2 = x2 *. q.w in let yy2 = y2 *. q.y in let yz2 = y2 *. q.z in
    let yw2 = y2 *. q.w in let zz2 = z2 *. q.z in let zw2 = z2 *. q.w in v
      (1.0 -. yy2 -. zz2) (xy2 -. zw2)        (xz2 +. yw2)
      (xy2 +. zw2)        (1.0 -. xx2 -. zz2) (yz2 -. xw2)
      (xz2 -. yw2)        (yz2 +. xw2)        (1.0 -. xx2 -. yy2)

  (* Functions *)

  let neg a =
    v (-. a.e00) (-. a.e01) (-. a.e02)
      (-. a.e10) (-. a.e11) (-. a.e12)
      (-. a.e20) (-. a.e21) (-. a.e22)

  let add a b =
    v (a.e00 +. b.e00) (a.e01 +. b.e01) (a.e02 +. b.e02)
      (a.e10 +. b.e10) (a.e11 +. b.e11) (a.e12 +. b.e12)
      (a.e20 +. b.e20) (a.e21 +. b.e21) (a.e22 +. b.e22)

  let sub a b =
    v (a.e00 -. b.e00) (a.e01 -. b.e01) (a.e02 -. b.e02)
      (a.e10 -. b.e10) (a.e11 -. b.e11) (a.e12 -. b.e12)
      (a.e20 -. b.e20) (a.e21 -. b.e21) (a.e22 -. b.e22)

  let mul a b =
    if a == id then b else
    if b == id then a else
      v (a.e00 *. b.e00 +. a.e01 *. b.e10 +. a.e02 *. b.e20)
        (a.e00 *. b.e01 +. a.e01 *. b.e11 +. a.e02 *. b.e21)
        (a.e00 *. b.e02 +. a.e01 *. b.e12 +. a.e02 *. b.e22)
        (a.e10 *. b.e00 +. a.e11 *. b.e10 +. a.e12 *. b.e20)
        (a.e10 *. b.e01 +. a.e11 *. b.e11 +. a.e12 *. b.e21)
        (a.e10 *. b.e02 +. a.e11 *. b.e12 +. a.e12 *. b.e22)
        (a.e20 *. b.e00 +. a.e21 *. b.e10 +. a.e22 *. b.e20)
        (a.e20 *. b.e01 +. a.e21 *. b.e11 +. a.e22 *. b.e21)
        (a.e20 *. b.e02 +. a.e21 *. b.e12 +. a.e22 *. b.e22)

  let emul a b =
    v (a.e00 *. b.e00) (a.e01 *. b.e01) (a.e02 *. b.e02)
      (a.e10 *. b.e10) (a.e11 *. b.e11) (a.e12 *. b.e12)
      (a.e20 *. b.e20) (a.e21 *. b.e21) (a.e22 *. b.e22)

  let ediv a b =
    v (a.e00 /. b.e00) (a.e01 /. b.e01) (a.e02 /. b.e02)
      (a.e10 /. b.e10) (a.e11 /. b.e11) (a.e12 /. b.e12)
      (a.e20 /. b.e20) (a.e21 /. b.e21) (a.e22 /. b.e22)

  let smul s a =
    v (s *. a.e00) (s *. a.e01) ( s *. a.e02)
      (s *. a.e10) (s *. a.e11) ( s *. a.e12)
      (s *. a.e20) (s *. a.e21) ( s *. a.e22)

  let transpose a =
    v a.e00 a.e10 a.e20
      a.e01 a.e11 a.e21
      a.e02 a.e12 a.e22

  let trace a = a.e00 +. a.e11 +. a.e22
  let det a =
    let m00 = (a.e11 *. a.e22) -. (a.e21 *. a.e12) in               (* minor. *)
    let m10 = (a.e01 *. a.e22) -. (a.e21 *. a.e02) in
    let m20 = (a.e01 *. a.e12) -. (a.e11 *. a.e02) in
    (a.e00 *. m00) -. (a.e10 *. m10) +. (a.e20 *. m20)

  let inv a =
    let m00 = (a.e11 *. a.e22) -. (a.e21 *. a.e12) in               (* minor. *)
    let m10 = (a.e01 *. a.e22) -. (a.e21 *. a.e02) in
    let m20 = (a.e01 *. a.e12) -. (a.e11 *. a.e02) in
    let m01 = (a.e10 *. a.e22) -. (a.e20 *. a.e12) in
    let m11 = (a.e00 *. a.e22) -. (a.e20 *. a.e02) in
    let m21 = (a.e00 *. a.e12) -. (a.e10 *. a.e02) in
    let m02 = (a.e10 *. a.e21) -. (a.e20 *. a.e11) in
    let m12 = (a.e00 *. a.e21) -. (a.e20 *. a.e01) in
    let m22 = (a.e00 *. a.e11) -. (a.e10 *. a.e01) in
    let det = (a.e00 *. m00) -. (a.e10 *. m10) +. (a.e20 *. m20) in
    v (   m00 /. det) (-. m10 /. det) (   m20 /. det)
      (-. m01 /. det) (   m11 /. det) (-. m21 /. det)
      (   m02 /. det) (-. m12 /. det) (   m22 /. det)

  (* 2D space transforms *)

  let move2 d =
    v 1. 0. d.V2t.x
      0. 1. d.V2t.y
      0. 0. 1.

  let rot2 ?pt theta =
    let c = cos theta in
    let s = sin theta in
    match pt with
    | None ->
      v c  (-. s) 0.
        s  c      0.
        0. 0.     1.
    | Some pt ->
      let px = P2.x pt in
      let py = P2.y pt in
      v c  (-. s) (-. c *. px +. s *. py +. px)
        s  c      (-. s *. px -. c *. py +. py)
        0. 0.     1.

  let scale2 s =
    v s.V2t.x 0.      0.
      0.      s.V2t.y 0.
      0.      0.      1.

  let rigid2 ~move ~rot =
    let c = cos rot in
    let s = sin rot in
    v c  (-. s) move.V2t.x
      s  c      move.V2t.y
      0. 0.     1.

  let srigid2 ~move ~rot ~scale =
    let c = cos rot in
    let s = sin rot in
    v (c *. scale.V2t.x) ((-. s) *. scale.V2t.y) move.V2t.x
      (s *. scale.V2t.x) (c *. scale.V2t.y)      move.V2t.y
      0.                 0.                      1.

  (* 3D space transforms *)

  let rot3_map u u' =
    let n = V3.cross u u' in
    let e = V3.dot u u' in
    let h = 1. /. (1. +. e) in
    let xy = n.x *. n.y in
    let xz = n.x *. n.z in
    let yz = n.y *. n.z in
    v (e +. h *. n.x *. n.x) (h *. xy -. n.z)       (h *. xz +. n.y)
      (h *. xy +. n.z)       (e +. h *. n.y *. n.y) (h *. yz -. n.x)
      (h *. xz -. n.y)       (h *. yz +. n.x)       (e +. h *. n.z *. n.z)

  let rot3_axis u theta =
    let xy = u.x *. u.y in
    let xz = u.x *. u.z in
    let yz = u.y *. u.z in
    let c = (cos theta) in
    let one_c = 1. -. c in
    let s = (sin theta) in
    v (u.x *. u.x *. one_c +. c)
      (xy *. one_c -. u.z *. s)
      (xz *. one_c +. u.y *. s)
      (xy *. one_c +. u.z *. s)
      (u.y *. u.y *. one_c +. c)
      (yz *. one_c -. u.x *. s)
      (xz *. one_c -. u.y *. s)
      (yz *. one_c +. u.x *. s)
      (u.z *. u.z *. one_c +. c)

  let rot3_zyx r =
    let cz = cos r.z in let sz = sin r.z in
    let cy = cos r.y in let sy = sin r.y in
    let cx = cos r.x in let sx = sin r.x in
    v (cy *. cz) (sy *. sx *. cz -. cx *. sz) (sy *. cx *. cz +. sx *. sz)
      (cy *. sz) (sy *. sx *. sz +. cx *. cz) (sy *. cx *. sz -. sx *. cz)
      (-. sy)    (cy *. sx)                   (cy *. cx)

  let scale3 s =
    v s.x 0.  0.
      0.  s.y 0.
      0.  0.  s.z

  (* Traversal *)

  let map f a =
    v (f a.e00) (f a.e01) (f a.e02)
      (f a.e10) (f a.e11) (f a.e12)
      (f a.e20) (f a.e21) (f a.e22)

  let mapi f a =
    v (f 0 0 a.e00) (f 0 1 a.e01) (f 0 2 a.e02)
      (f 1 0 a.e10) (f 1 1 a.e11) (f 1 2 a.e12)
      (f 2 0 a.e20) (f 2 1 a.e21) (f 2 2 a.e22)

  let fold f acc a =
    f (f (f (f (f (f (f (f (f acc a.e00) a.e10) a.e20) a.e01) a.e11) a.e21)
            a.e02) a.e12) a.e22

  let foldi f acc a =
    f (f (f (f (f (f (f (f (f acc 0 0 a.e00) 1 0 a.e10) 2 0 a.e20) 0 1 a.e01)
                  1 1 a.e11) 2 1 a.e21) 0 2 a.e02) 1 2 a.e12) 2 2 a.e22

  let iter f a =
    f a.e00; f a.e10; f a.e20;
    f a.e01; f a.e11; f a.e21;
    f a.e02; f a.e12; f a.e22

  let iteri f a =
    f 0 0 a.e00; f 1 0 a.e10; f 2 0 a.e20;
    f 0 1 a.e01; f 1 1 a.e11; f 2 1 a.e21;
    f 0 2 a.e02; f 1 2 a.e12; f 2 2 a.e22

  (* Predicates and comparisons *)

  let for_all p a =
    p a.e00 && p a.e10 && p a.e20 &&
    p a.e01 && p a.e11 && p a.e21 &&
    p a.e02 && p a.e12 && p a.e22

  let exists p a =
    p a.e00 || p a.e10 || p a.e20 ||
    p a.e01 || p a.e11 || p a.e21 ||
    p a.e02 || p a.e12 || p a.e22

  let equal = (=)
  let equal_f eq a b =
    eq a.e00 b.e00 && eq a.e10 b.e10 && eq a.e20 b.e20 &&
    eq a.e01 b.e01 && eq a.e11 b.e11 && eq a.e21 b.e21 &&
    eq a.e02 b.e02 && eq a.e12 b.e12 && eq a.e22 b.e22

  let compare = Pervasives.compare
  let compare_f cmp a b =
    let c = cmp a.e00 b.e00 in if c <> 0 then c else
      let c = cmp a.e10 b.e10 in if c <> 0 then c else
        let c = cmp a.e20 b.e20 in if c <> 0 then c else
          let c = cmp a.e01 b.e01 in if c <> 0 then c else
            let c = cmp a.e11 b.e11 in if c <> 0 then c else
              let c = cmp a.e21 b.e21 in if c <> 0 then c else
                let c = cmp a.e02 b.e02 in if c <> 0 then c else
                  let c = cmp a.e12 b.e12 in if c <> 0 then c else
                    let c = cmp a.e22 b.e22 in c

  (* Printers *)

  let pp_f pp_e ppf a =
    let max : int -> int -> int -> int = fun a b c ->
      if a > b then (if a > c then a else c) else (if b > c then b else c)
    in
    let b = Buffer.create 30 in
    let bppf = Format.formatter_of_buffer b in
    let e00, e00l = pp_buf b bppf "%a" pp_e a.e00 in
    let e10, e10l = pp_buf b bppf "%a" pp_e a.e10 in
    let e20, e20l = pp_buf b bppf "%a" pp_e a.e20 in
    let max0 = max e00l e10l e20l in
    let e01, e01l = pp_buf b bppf "%a" pp_e a.e01 in
    let e11, e11l = pp_buf b bppf "%a" pp_e a.e11 in
    let e21, e21l = pp_buf b bppf "%a" pp_e a.e21 in
    let max1 = max e01l e11l e21l in
    let e02, e02l = pp_buf b bppf "%a" pp_e a.e02 in
    let e12, e12l = pp_buf b bppf "%a" pp_e a.e12 in
    let e22, e22l = pp_buf b bppf "%a" pp_e a.e22 in
    let max2 = max e02l e12l e22l in
    pp ppf
      "@[<v>@[<1>|%a%s@ %a%s@ %a%s|@]@,\
       @[<1>|%a%s@ %a%s@ %a%s|@]@,\
       @[<1>|%a%s@ %a%s@ %a%s|@]@]"
      pp_pad (max0 - e00l) e00 pp_pad (max1 - e01l) e01 pp_pad (max2 - e02l) e02
      pp_pad (max0 - e10l) e10 pp_pad (max1 - e11l) e11 pp_pad (max2 - e12l) e12
      pp_pad (max0 - e20l) e20 pp_pad (max1 - e21l) e21 pp_pad (max2 - e22l) e22


  let pp_e_default ppf = pp ppf "%g"
  let pp ppf a = pp_f pp_e_default ppf a
end

module M4 = struct
  open M4t
  open V4t
  type t = m4
  let dim = 4
  type v = v4

  (* Constructors, accessors and constants *)

  let v e00 e01 e02 e03 e10 e11 e12 e13 e20 e21 e22 e23 e30 e31 e32 e33 =
    { e00 = e00; e10 = e10; e20 = e20; e30 = e30;
      e01 = e01; e11 = e11; e21 = e21; e31 = e31;
      e02 = e02; e12 = e12; e22 = e22; e32 = e32;
      e03 = e03; e13 = e13; e23 = e23; e33 = e33 }

  let of_rows r0 r1 r2 r3 =
    v r0.x r0.y r0.z r0.w
      r1.x r1.y r1.z r1.w
      r2.x r2.y r2.z r2.w
      r3.x r3.y r3.z r3.w

  let of_cols c0 c1 c2 c3 =
    v c0.x c1.x c2.x c3.x
      c0.y c1.y c2.y c3.y
      c0.z c1.z c2.z c3.z
      c0.w c1.w c2.w c3.w

  let el row col = M4t.i.(dim * col + row)
  let e00 a = a.e00
  let e01 a = a.e01
  let e02 a = a.e02
  let e03 a = a.e03
  let e10 a = a.e10
  let e11 a = a.e11
  let e12 a = a.e12
  let e13 a = a.e13
  let e20 a = a.e20
  let e21 a = a.e21
  let e22 a = a.e22
  let e23 a = a.e23
  let e30 a = a.e30
  let e31 a = a.e31
  let e32 a = a.e32
  let e33 a = a.e33
  let row r = M4t.row.(r)
  let col c = M4t.col.(c)
  let zero = v 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.
  let id = v 1. 0. 0. 0. 0. 1. 0. 0. 0. 0. 1. 0. 0. 0. 0. 1.
  let of_m3_v3 a u =
    v a.M3t.e00 a.M3t.e01 a.M3t.e02 u.V3t.x
      a.M3t.e10 a.M3t.e11 a.M3t.e12 u.V3t.y
      a.M3t.e20 a.M3t.e21 a.M3t.e22 u.V3t.z
      0.        0.        0.        1.

  let of_quat q =                   (* NOTE, code duplicate with M3.of_quat. *)
    let x2 = q.x +. q.x in let y2 = q.y +. q.y in let z2 = q.z +. q.z in
    let xx2 = x2 *. q.x in let xy2 = x2 *. q.y in let xz2 = x2 *. q.z in
    let xw2 = x2 *. q.w in let yy2 = y2 *. q.y in let yz2 = y2 *. q.z in
    let yw2 = y2 *. q.w in let zz2 = z2 *. q.z in let zw2 = z2 *. q.w in v
      (1.0 -. yy2 -. zz2) (xy2 -. zw2)         (xz2 +. yw2)          0.0
      (xy2 +. zw2)        (1.0 -. xx2 -. zz2)  (yz2 -. xw2)          0.0
      (xz2 -. yw2)        (yz2 +. xw2)         (1.0 -. xx2 -. yy2)   0.0
      0.0                 0.0                  0.0                   1.0

  (* Functions *)

  let neg a =
    v (-. a.e00) (-. a.e01) (-. a.e02) (-. a.e03)
      (-. a.e10) (-. a.e11) (-. a.e12) (-. a.e13)
      (-. a.e20) (-. a.e21) (-. a.e22) (-. a.e23)
      (-. a.e30) (-. a.e31) (-. a.e32) (-. a.e33)

  let add a b =
    v (a.e00 +. b.e00) (a.e01 +. b.e01) (a.e02 +. b.e02) (a.e03 +. b.e03)
      (a.e10 +. b.e10) (a.e11 +. b.e11) (a.e12 +. b.e12) (a.e13 +. b.e13)
      (a.e20 +. b.e20) (a.e21 +. b.e21) (a.e22 +. b.e22) (a.e23 +. b.e23)
      (a.e30 +. b.e30) (a.e31 +. b.e31) (a.e32 +. b.e32) (a.e33 +. b.e33)

  let sub a b =
    v (a.e00 -. b.e00) (a.e01 -. b.e01) (a.e02 -. b.e02) (a.e03 -. b.e03)
      (a.e10 -. b.e10) (a.e11 -. b.e11) (a.e12 -. b.e12) (a.e13 -. b.e13)
      (a.e20 -. b.e20) (a.e21 -. b.e21) (a.e22 -. b.e22) (a.e23 -. b.e23)
      (a.e30 -. b.e30) (a.e31 -. b.e31) (a.e32 -. b.e32) (a.e33 -. b.e33)

  let mul a b =
    if a == id then b else
    if b == id then a else
      v (a.e00 *. b.e00 +. a.e01 *. b.e10 +. a.e02 *. b.e20 +. a.e03 *. b.e30)
        (a.e00 *. b.e01 +. a.e01 *. b.e11 +. a.e02 *. b.e21 +. a.e03 *. b.e31)
        (a.e00 *. b.e02 +. a.e01 *. b.e12 +. a.e02 *. b.e22 +. a.e03 *. b.e32)
        (a.e00 *. b.e03 +. a.e01 *. b.e13 +. a.e02 *. b.e23 +. a.e03 *. b.e33)
        (a.e10 *. b.e00 +. a.e11 *. b.e10 +. a.e12 *. b.e20 +. a.e13 *. b.e30)
        (a.e10 *. b.e01 +. a.e11 *. b.e11 +. a.e12 *. b.e21 +. a.e13 *. b.e31)
        (a.e10 *. b.e02 +. a.e11 *. b.e12 +. a.e12 *. b.e22 +. a.e13 *. b.e32)
        (a.e10 *. b.e03 +. a.e11 *. b.e13 +. a.e12 *. b.e23 +. a.e13 *. b.e33)
        (a.e20 *. b.e00 +. a.e21 *. b.e10 +. a.e22 *. b.e20 +. a.e23 *. b.e30)
        (a.e20 *. b.e01 +. a.e21 *. b.e11 +. a.e22 *. b.e21 +. a.e23 *. b.e31)
        (a.e20 *. b.e02 +. a.e21 *. b.e12 +. a.e22 *. b.e22 +. a.e23 *. b.e32)
        (a.e20 *. b.e03 +. a.e21 *. b.e13 +. a.e22 *. b.e23 +. a.e23 *. b.e33)
        (a.e30 *. b.e00 +. a.e31 *. b.e10 +. a.e32 *. b.e20 +. a.e33 *. b.e30)
        (a.e30 *. b.e01 +. a.e31 *. b.e11 +. a.e32 *. b.e21 +. a.e33 *. b.e31)
        (a.e30 *. b.e02 +. a.e31 *. b.e12 +. a.e32 *. b.e22 +. a.e33 *. b.e32)
        (a.e30 *. b.e03 +. a.e31 *. b.e13 +. a.e32 *. b.e23 +. a.e33 *. b.e33)

  let emul a b =
    v (a.e00 *. b.e00) (a.e01 *. b.e01) (a.e02 *. b.e02) (a.e03 *. b.e03)
      (a.e10 *. b.e10) (a.e11 *. b.e11) (a.e12 *. b.e12) (a.e13 *. b.e13)
      (a.e20 *. b.e20) (a.e21 *. b.e21) (a.e22 *. b.e22) (a.e23 *. b.e23)
      (a.e30 *. b.e30) (a.e31 *. b.e31) (a.e32 *. b.e32) (a.e33 *. b.e33)

  let ediv a b =
    v (a.e00 /. b.e00) (a.e01 /. b.e01) (a.e02 /. b.e02) (a.e03 /. b.e03)
      (a.e10 /. b.e10) (a.e11 /. b.e11) (a.e12 /. b.e12) (a.e13 /. b.e13)
      (a.e20 /. b.e20) (a.e21 /. b.e21) (a.e22 /. b.e22) (a.e23 /. b.e23)
      (a.e30 /. b.e30) (a.e31 /. b.e31) (a.e32 /. b.e32) (a.e33 /. b.e33)

  let smul s a =
    v (s *. a.e00) (s *. a.e01) (s *. a.e02) (s *. a.e03)
      (s *. a.e10) (s *. a.e11) (s *. a.e12) (s *. a.e13)
      (s *. a.e20) (s *. a.e21) (s *. a.e22) (s *. a.e23)
      (s *. a.e30) (s *. a.e31) (s *. a.e32) (s *. a.e33)

  let transpose a =
    v a.e00 a.e10 a.e20 a.e30
      a.e01 a.e11 a.e21 a.e31
      a.e02 a.e12 a.e22 a.e32
      a.e03 a.e13 a.e23 a.e33

  let trace a = a.e00 +. a.e11 +. a.e22 +. a.e33
  let det a =
    let d1 = (a.e22 *. a.e33) -. (a.e32 *. a.e23) in        (* second minor. *)
    let d2 = (a.e21 *. a.e33) -. (a.e31 *. a.e23) in
    let d3 = (a.e21 *. a.e32) -. (a.e31 *. a.e22) in
    let m00 = (a.e11 *. d1) -. (a.e12 *. d2) +. (a.e13 *. d3) in   (* minor. *)
    let m10 = (a.e01 *. d1) -. (a.e02 *. d2) +. (a.e03 *. d3) in
    let d4 = (a.e02 *. a.e13) -. (a.e12 *. a.e03) in
    let d5 = (a.e01 *. a.e13) -. (a.e11 *. a.e03) in
    let d6 = (a.e01 *. a.e12) -. (a.e11 *. a.e02) in
    let m20 = (a.e31 *. d4) -. (a.e32 *. d5) +. (a.e33 *. d6) in
    let m30 = (a.e21 *. d4) -. (a.e22 *. d5) +. (a.e23 *. d6) in
    (a.e00 *. m00) -. (a.e10 *. m10) +. (a.e20 *. m20) -. (a.e30 *. m30)

  let inv a =
    let d1 = (a.e22 *. a.e33) -. (a.e32 *. a.e23) in        (* second minor. *)
    let d2 = (a.e21 *. a.e33) -. (a.e31 *. a.e23) in
    let d3 = (a.e21 *. a.e32) -. (a.e31 *. a.e22) in
    let m00 = (a.e11 *. d1) -. (a.e12 *. d2) +. (a.e13 *. d3) in   (* minor. *)
    let m10 = (a.e01 *. d1) -. (a.e02 *. d2) +. (a.e03 *. d3) in
    let d4 = (a.e02 *. a.e13) -. (a.e12 *. a.e03) in
    let d5 = (a.e01 *. a.e13) -. (a.e11 *. a.e03) in
    let d6 = (a.e01 *. a.e12) -. (a.e11 *. a.e02) in
    let m20 = (a.e31 *. d4) -. (a.e32 *. d5) +. (a.e33 *. d6) in
    let m30 = (a.e21 *. d4) -. (a.e22 *. d5) +. (a.e23 *. d6) in
    let d7 = (a.e20 *. a.e33) -. (a.e30 *. a.e23) in
    let d8 = (a.e20 *. a.e32) -. (a.e30 *. a.e22) in
    let m01 = (a.e10 *. d1) -. (a.e12 *. d7) +. (a.e13 *. d8) in
    let m11 = (a.e00 *. d1) -. (a.e02 *. d7) +. (a.e03 *. d8) in
    let d9 = (a.e00 *. a.e13) -. (a.e10 *. a.e03) in
    let d10 = (a.e00 *. a.e12) -. (a.e10 *. a.e02) in
    let m21 = (a.e30 *. d4) -. (a.e32 *. d9) +. (a.e33 *. d10) in
    let m31 = (a.e20 *. d4) -. (a.e22 *. d9) +. (a.e23 *. d10) in
    let d11 = (a.e20 *. a.e31) -. (a.e30 *. a.e21) in
    let m02 = (a.e10 *. d2) -. (a.e11 *. d7) +. (a.e13 *. d11) in
    let m12 = (a.e00 *. d2) -. (a.e01 *. d7) +. (a.e03 *. d11) in
    let d12 = (a.e00 *. a.e11) -. (a.e10 *. a.e01) in
    let m22 = (a.e30 *. d5) -. (a.e31 *. d9) +. (a.e33 *. d12) in
    let m32  =(a.e20 *. d5) -. (a.e21 *. d9) +. (a.e23 *. d12) in
    let m03 = (a.e10 *. d3) -. (a.e11 *. d8) +. (a.e12 *. d11) in
    let m13 = (a.e00 *. d3) -. (a.e01 *. d8) +. (a.e02 *. d11) in
    let m23 = (a.e30 *. d6) -. (a.e31 *. d10) +. (a.e32 *. d12) in
    let m33 = (a.e20 *. d6) -. (a.e21 *. d10) +. (a.e22 *. d12) in
    let det =
      (a.e00 *. m00) -. (a.e10 *. m10) +. (a.e20 *. m20) -. (a.e30 *. m30)
    in
    v (   m00 /. det) (-. m10 /. det) (   m20 /. det) (-. m30 /. det)
      (-. m01 /. det) (   m11 /. det) (-. m21 /. det) (   m31 /. det)
      (   m02 /. det) (-. m12 /. det) (   m22 /. det) (-. m32 /. det)
      (-. m03 /. det) (   m13 /. det) (-. m23 /. det) (   m33 /. det)

  (* 2D space transforms *)

  (* 2D space transforms *)

  let move2 d =
    v 1. 0. 0. d.V2t.x
      0. 1. 0. d.V2t.y
      0. 0. 1. 0.
      0. 0. 0. 1.

  let rot2 ?pt theta =
    let c = cos theta in
    let s = sin theta in
    match pt with
    | None ->
      v c  (-. s) 0. 0.
        s  c      0. 0.
        0. 0.     1. 0.
        0. 0.     0. 1.
    | Some pt ->
      let px = P2.x pt in
      let py = P2.y pt in
      v c  (-. s) (-. c *. px +. s *. py +. px) 0.
        s  c      (-. s *. px -. c *. py +. py) 0.
        0. 0.     1.                            0.
        0. 0.     0.                            1.

  let scale2 s =
    v s.V2t.x 0.      0. 0.
      0.      s.V2t.y 0. 0.
      0.      0.      1. 0.
      0.      0.      0. 1.

  let rigid2 ~move ~rot =
    let c = cos rot in
    let s = sin rot in
    v c  (-. s) 0. move.V2t.x
      s  c      0. move.V2t.y
      0. 0.     1. 0.
      0. 0.     0. 1.

  let srigid2 ~move ~rot ~scale =
    let c = cos rot in
    let s = sin rot in
    v (c *. scale.V2t.x) ((-. s) *. scale.V2t.y) 0. move.V2t.x
      (s *. scale.V2t.x) (c *. scale.V2t.y)      0. move.V2t.y
      0.                 0.                      1. 0.
      0.                 0.                      0. 1.

  (* 3D space transforms *)

  let move3 d =
    v 1. 0. 0. d.V3t.x
      0. 1. 0. d.V3t.y
      0. 0. 1. d.V3t.z
      0. 0. 0. 1.

  let rot3_map u u' =
    let n = V3.cross u u' in
    let e = V3.dot u u' in
    let h = 1. /. (1. +. e) in
    let x = n.V3t.x in
    let y = n.V3t.y in
    let z = n.V3t.z in
    let xy = x *. y in
    let xz = x *. z in
    let yz = y *. z in
    v (e +. h *. x *. x) (h *. xy -. z)       (h *. xz +. y)     0.
      (h *. xy +. z)     (e +. h *. y *. y)   (h *. yz -. x)     0.
      (h *. xz -. y)     (h *. yz +. x)       (e +. h *. z *. z) 0.
      0.                 0.                   0.                 1.

  let rot3_axis u theta =
    let xy = u.V3t.x *. u.V3t.y in
    let xz = u.V3t.x *. u.V3t.z in
    let yz = u.V3t.y *. u.V3t.z in
    let c = (cos theta) in
    let one_c = 1. -. c in
    let s = (sin theta) in
    v (u.V3t.x *. u.V3t.x *. one_c +. c)
      (xy *. one_c -. u.V3t.z *. s)
      (xz *. one_c +. u.V3t.y *. s)
      0.
      (xy *. one_c +. u.V3t.z *. s)
      (u.V3t.y *. u.V3t.y *. one_c +. c)
      (yz *. one_c -. u.V3t.x *. s)
      0.
      (xz *. one_c -. u.V3t.y *. s)
      (yz *. one_c +. u.V3t.x *. s)
      (u.V3t.z *. u.V3t.z *. one_c +. c)
      0.
      0. 0. 0. 1.

  let rot3_zyx r =
    let cz = cos r.V3t.z in let sz = sin r.V3t.z in
    let cy = cos r.V3t.y in let sy = sin r.V3t.y in
    let cx = cos r.V3t.x in let sx = sin r.V3t.x in
    v (cy *. cz) (sy *. sx *. cz -. cx *. sz) (sy *. cx *. cz +. sx *. sz) 0.
      (cy *. sz) (sy *. sx *. sz +. cx *. cz) (sy *. cx *. sz -. sx *. cz) 0.
      (-. sy)    (cy *. sx)                   (cy *. cx)                   0.
      0.         0.                           0.                           1.

  let scale3 s =
    v s.V3t.x 0.      0.      0.
      0.      s.V3t.y 0.      0.
      0.      0.      s.V3t.z 0.
      0.      0.      0.      1.

  let rigid3 ~move:d ~rot:(u, theta) =
    { (rot3_axis u theta) with e03 = d.V3t.x; e13 = d.V3t.y; e23 = d.V3t.z }

  let rigid3q ~move:d ~rot:q =
    { (of_quat q) with e03 = d.V3t.x; e13 = d.V3t.y; e23 = d.V3t.z }

  let _srigid d m s =
    v (m.e00 *. s.V3t.x) (m.e01 *. s.V3t.y) (m.e02 *. s.V3t.z) d.V3t.x
      (m.e10 *. s.V3t.x) (m.e11 *. s.V3t.y) (m.e12 *. s.V3t.z) d.V3t.y
      (m.e20 *. s.V3t.x) (m.e21 *. s.V3t.y) (m.e22 *. s.V3t.z) d.V3t.z
      0.                 0.                 0.                 1.

  let srigid3 ~move:d ~rot:(u, a) ~scale:s = _srigid d (rot3_axis u a) s
  let srigid3q ~move:d ~rot:q ~scale:s = _srigid d (of_quat q) s

  let ortho ~left ~right ~bot ~top ~near ~far =
    let drl = 1. /. (right -. left) in
    let dtb = 1. /. (top -. bot) in
    let dfn = 1. /. (far -. near) in v
      (2. *. drl) 0.           0.             (-. (right +. left) *. drl)
      0.          (2. *. dtb)  0.             (-. (top +. bot) *. dtb)
      0.          0.           (-. 2. *. dfn) (-. (far +. near)   *. dfn)
      0.          0.           0.             1.0

  let persp ~left ~right ~bot ~top ~near ~far =
    let drl = 1. /. (right -. left) in
    let dtb = 1. /. (top -. bot) in
    let dfn = 1. /. (far -. near) in
    let n2 = 2. *. near in v
      (n2 *. drl)  0.          ((right +. left) *. drl)  0.
      0.           (n2 *. dtb) ((top +. bot) *. dtb)  0.
      0.           0.          (-. (far +. near) *. dfn) (-. (n2 *. far) *. dfn)
      0.           0.          (-. 1.)                   0.

(*
  let persp_fov ~fovy ~aspect ~near ~far =
    let f = 1. /. tan (fovy *. 0.5) in
    let dnf = 1. /. (near -. far) in v
      (f /. aspect) 0. 0.                     0.
      0.            f  0.                     0.
      0.            0. ((far +. near) *. dnf) ((2. *. far *. near) *. dnf)
      0.            0. (-. 1.)                0.
*)

  (* 4D space transforms *)

  let scale4 s =
    v s.x 0.  0.  0.
      0.  s.y 0.  0.
      0.  0.  s.z 0.
      0.  0.  0.  s.w

  (* Traversal *)

  let map f a =
    v (f a.e00) (f a.e01) (f a.e02) (f a.e03)
      (f a.e10) (f a.e11) (f a.e12) (f a.e13)
      (f a.e20) (f a.e21) (f a.e22) (f a.e23)
      (f a.e30) (f a.e31) (f a.e32) (f a.e33)

  let mapi f a =
    v (f 0 0 a.e00) (f 0 1 a.e01) (f 0 2 a.e02) (f 0 3 a.e03)
      (f 1 0 a.e10) (f 1 1 a.e11) (f 1 2 a.e12) (f 1 3 a.e13)
      (f 2 0 a.e20) (f 2 1 a.e21) (f 2 2 a.e22) (f 2 3 a.e23)
      (f 3 0 a.e30) (f 3 1 a.e31) (f 3 2 a.e32) (f 3 3 a.e33)

  let fold f acc a =
    f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f acc a.e00) a.e10)
                                             a.e20) a.e30) a.e01) a.e11) a.e21) a.e31) a.e02) a.e12) a.e22) a.e32)
               a.e03) a.e13) a.e23) a.e33

  let foldi f acc a =
    f (f (f (f (f (f (f (f (f (f (f (f (f (f (f (f acc 0 0 a.e00) 1 0 a.e10)
                                             2 0 a.e20) 3 0 a.e30) 0 1 a.e01) 1 1 a.e11) 2 1 a.e21) 3 1 a.e31) 0 2 a.e02)
                        1 2 a.e12) 2 2 a.e22) 3 2 a.e32) 0 3 a.e03) 1 3 a.e13) 2 3 a.e23) 3 3 a.e33

  let iter f a =
    f a.e00; f a.e10; f a.e20; f a.e30;
    f a.e01; f a.e11; f a.e21; f a.e31;
    f a.e02; f a.e12; f a.e22; f a.e32;
    f a.e03; f a.e13; f a.e23; f a.e33

  let iteri f a =
    f 0 0 a.e00; f 1 0 a.e10; f 2 0 a.e20; f 3 0 a.e30;
    f 0 1 a.e01; f 1 1 a.e11; f 2 1 a.e21; f 3 1 a.e31;
    f 0 2 a.e02; f 1 2 a.e12; f 2 2 a.e22; f 3 2 a.e32;
    f 0 3 a.e03; f 1 3 a.e13; f 2 3 a.e23; f 3 3 a.e33

  (* Predicates and comparisons *)

  let for_all p a =
    p a.e00 && p a.e10 && p a.e20 && p a.e30 &&
    p a.e01 && p a.e11 && p a.e21 && p a.e31 &&
    p a.e02 && p a.e12 && p a.e22 && p a.e32 &&
    p a.e03 && p a.e13 && p a.e23 && p a.e33

  let exists p a =
    p a.e00 || p a.e10 || p a.e20 || p a.e30 ||
    p a.e01 || p a.e11 || p a.e21 || p a.e31 ||
    p a.e02 || p a.e12 || p a.e22 || p a.e32 ||
    p a.e03 || p a.e13 || p a.e23 || p a.e33

  let equal = (=)
  let equal_f eq a b =
    eq a.e00 b.e00 && eq a.e10 b.e10 && eq a.e20 b.e20 && eq a.e30 b.e30 &&
    eq a.e01 b.e01 && eq a.e11 b.e11 && eq a.e21 b.e21 && eq a.e31 b.e31 &&
    eq a.e02 b.e02 && eq a.e12 b.e12 && eq a.e22 b.e22 && eq a.e32 b.e32 &&
    eq a.e03 b.e03 && eq a.e13 b.e13 && eq a.e23 b.e23 && eq a.e33 b.e33

  let compare = Pervasives.compare
  let compare_f cmp a b =
    let c = cmp a.e00 b.e00 in if c <> 0 then c else
      let c = cmp a.e10 b.e10 in if c <> 0 then c else
        let c = cmp a.e20 b.e20 in if c <> 0 then c else
          let c = cmp a.e30 b.e30 in if c <> 0 then c else
            let c = cmp a.e01 b.e01 in if c <> 0 then c else
              let c = cmp a.e11 b.e11 in if c <> 0 then c else
                let c = cmp a.e21 b.e21 in if c <> 0 then c else
                  let c = cmp a.e31 b.e31 in if c <> 0 then c else
                    let c = cmp a.e02 b.e02 in if c <> 0 then c else
                      let c = cmp a.e12 b.e12 in if c <> 0 then c else
                        let c = cmp a.e22 b.e22 in if c <> 0 then c else
                          let c = cmp a.e32 b.e32 in if c <> 0 then c else
                            let c = cmp a.e03 b.e03 in if c <> 0 then c else
                              let c = cmp a.e13 b.e13 in if c <> 0 then c else
                                let c = cmp a.e23 b.e23 in if c <> 0 then c else
                                  let c = cmp a.e33 b.e33 in c

  (* Printers *)

  let pp_f pp_e ppf a =
    let max : int -> int -> int -> int -> int = fun a b c d ->
      let max1 = if a > b then a else b in
      let max2 = if c > d then c else d in
      if max1 > max2 then max1 else max2
    in
    let b = Buffer.create 30 in
    let bppf = Format.formatter_of_buffer b in
    let e00, e00l = pp_buf b bppf "%a" pp_e a.e00 in
    let e10, e10l = pp_buf b bppf "%a" pp_e a.e10 in
    let e20, e20l = pp_buf b bppf "%a" pp_e a.e20 in
    let e30, e30l = pp_buf b bppf "%a" pp_e a.e30 in
    let max0 = max e00l e10l e20l e30l in
    let e01, e01l = pp_buf b bppf "%a" pp_e a.e01 in
    let e11, e11l = pp_buf b bppf "%a" pp_e a.e11 in
    let e21, e21l = pp_buf b bppf "%a" pp_e a.e21 in
    let e31, e31l = pp_buf b bppf "%a" pp_e a.e31 in
    let max1 = max e01l e11l e21l e31l in
    let e02, e02l = pp_buf b bppf "%a" pp_e a.e02 in
    let e12, e12l = pp_buf b bppf "%a" pp_e a.e12 in
    let e22, e22l = pp_buf b bppf "%a" pp_e a.e22 in
    let e32, e32l = pp_buf b bppf "%a" pp_e a.e32 in
    let max2 = max e02l e12l e22l e32l in
    let e03, e03l = pp_buf b bppf "%a" pp_e a.e03 in
    let e13, e13l = pp_buf b bppf "%a" pp_e a.e13 in
    let e23, e23l = pp_buf b bppf "%a" pp_e a.e23 in
    let e33, e33l = pp_buf b bppf "%a" pp_e a.e33 in
    let max3 = max e03l e13l e23l e33l in
    pp ppf
      "@[<v>@[<1>|%a%s@ %a%s@ %a%s@ %a%s|@]@,\
       @[<1>|%a%s@ %a%s@ %a%s@ %a%s|@]@,\
       @[<1>|%a%s@ %a%s@ %a%s@ %a%s|@]@,\
       @[<1>|%a%s@ %a%s@ %a%s@ %a%s|@]@]"
      pp_pad (max0 - e00l) e00 pp_pad (max1 - e01l) e01
      pp_pad (max2 - e02l) e02 pp_pad (max3 - e03l) e03
      pp_pad (max0 - e10l) e10 pp_pad (max1 - e11l) e11
      pp_pad (max2 - e12l) e12 pp_pad (max3 - e13l) e13
      pp_pad (max0 - e20l) e20 pp_pad (max1 - e21l) e21
      pp_pad (max2 - e22l) e22 pp_pad (max3 - e23l) e23
      pp_pad (max0 - e30l) e30 pp_pad (max1 - e31l) e31
      pp_pad (max2 - e32l) e32 pp_pad (max3 - e33l) e33

  let pp_e_default ppf = pp ppf "%g"
  let pp ppf a = pp_f pp_e_default ppf a
end

(* Sizes *)

type size1 = float
type size2 = v2
type size3 = v3

module type Size = sig
  type t
  val dim : int
  val zero : t
  val unit : t
end

module Size1 = struct
  type t = float
  let dim = 1
  let v w = w
  let w w = w
  let zero = 0.
  let unit = 1.
end

module Size2 = struct
  type t = size2
  let dim = 2
  let v = V2.v
  let w = V2.x
  let h = V2.y
  let zero = V2.zero
  let unit = V2.v 1. 1.
  let aspect s = s.V2t.x /. s.V2t.y
  let of_w w ~aspect = V2.v w (w /. aspect)
  let of_h h ~aspect = V2.v (h *. aspect) h
end

module Size3 = struct
  type t = size3
  let dim = 3
  let v = V3.v
  let w = V3.x
  let h = V3.y
  let d = V3.z
  let zero = V3.zero
  let unit = V3.v 1. 1. 1.
end

(* Axis aligned boxes *)

module type Box = sig
  type t
  val dim : int
  type v
  type p
  type size
  type m

  (* Constructors, accessors and constants *)

  val v : p -> size -> t
  val v_mid : p -> size -> t
  val empty : t
  val o : t -> p
  val size : t -> size
  val zero : t
  val unit : t
  val of_pts : p -> p -> t
  val add_pt : t -> p -> t

  (* Functions *)

  val min : t -> p
  val max : t -> p
  val mid : t -> p
  val area : t -> float
  val inter : t -> t -> t
  val union : t -> t -> t
  val inset : v -> t -> t
  val round : t -> t
  val move : v -> t -> t
  val ltr : m -> t -> t
  val map_f : (float -> float) -> t -> t

  (* Predicates and comparisons *)

  val is_empty : t -> bool
  val is_pt : t -> bool
  val isects : t -> t -> bool
  val subset : t -> t -> bool
  val mem : p -> t -> bool
  val equal : t -> t -> bool
  val equal_f : (float -> float -> bool) -> t -> t -> bool
  val compare : t -> t -> int
  val compare_f : (float -> float -> int) -> t -> t -> int

  (* Printers *)

  val pp : Format.formatter -> t -> unit
  val pp_f : (Format.formatter -> float -> unit) -> Format.formatter ->
    t -> unit
end

module Box1 = struct
  type t = E | R of float * float
  let dim = 1
  type v = float
  type p = float
  type size = size1
  type m = float

  let err_e () = invalid_arg err_empty_box

  (* Constructors, accessors and constants *)

  let v o s = R (o, s)
  let v_mid m s = R (m -. 0.5 *. s, s)
  let empty = E
  let o = function E -> err_e () | R (o, _) -> o
  let ox = function E -> err_e () | R (o, _) -> o
  let size = function E -> err_e () | R (_, size) -> size
  let w = function E -> err_e () | R (_, size) -> size
  let zero = v 0. Size1.zero
  let unit = v 0. Size1.unit
  let of_pts p p' = if p < p' then v p (p' -. p) else v p' (p -. p')
  let add_pt b p = match b with
    | E -> v p Size1.zero
    | R (o, s) as b ->
      let min = o in
      let max = o +. s in
      if p < min then R (p, max -. p) else
      if p > max then R (o, p -. min) else
        b

  (* Functions *)

  let min = o
  let minx = o
  let max = function E -> err_e () | R (o, s) -> o +. s
  let maxx = max
  let mid = function E -> err_e () | R (o, s) -> o +. 0.5 *. s
  let midx = mid
  let left = minx
  let right = maxx
  let area = function E -> 0. | R (_, s) -> s
  let inter b b' = match b, b' with
    | E, _ | _, E -> E
    | R (o, s), R (o', s') ->
      let l = o in let r = l +. s in
      let l' = o' in let r' = l' +. s' in
      if (r < l') || (r' < l) then E else
        let o'' = if l > l' then l else l' in
        let s'' = (if r < r' then r else r') -. o'' in
        v o'' s''

  let union b b' = match b, b' with
    | E, b | b, E -> b
    | R (o, s), R (o', s') ->
      let o'' = if o < o' then o else o' in
      let s'' =
        let r = o +. s in let r' = o' +. s' in
        (if r > r' then r else r') -. o''
      in
      v o'' s''

  let inset d = function
    | E -> E
    | R (o, s) ->
      let o' = o +. d in
      let s' = s -. 2. *. d in
      if s' < 0. then E else v o' s'

  let round = function
    | E -> E
    | R (o, s) ->
      let o' = floor o in
      let s' = if (s = 0. && o' <> o) then 1. else ceil s in
      v o' s'

  let move d = function E -> E | R (o, s) -> v (o +. d) s
  let ltr m = function E -> E | R (o, s) ->
    let c0 = m *. o in
    let c1 = m *. (o +. s) in
    if c0 < c1 then v c0 (c1 -. c0) else v c1 (c0 -. c1)

  let tr m = function E -> E | R (o, s) ->
    let c0 = m.M2t.e00 *. o +. m.M2t.e01 in
    let c1 = m.M2t.e00 *. (o +. s) +. m.M2t.e01 in
    if c0 < c1 then v c0 (c1 -. c0) else v c1 (c0 -. c1)

  let map_f f = function E -> E | R (o, s) -> v (f o) (f s)

  (* Predicates and comparisons *)

  let is_empty = function E -> true | R _ -> false
  let is_pt = function E -> false | R (_, s) -> s = 0.

  let isects b b' = match b, b' with
    | E, _ | _, E -> false
    | R (o, s), R (o', s') ->
      let l = o in let r = l +. s in
      let l' = o' in let r' = l' +. s' in
      not ((r < l') || (r' < l))

  let subset b b' = match b, b' with
    | _b, E -> false
    | E, _b -> true
    | R (o, s), R (o', s') -> (o' <= o) && (o +. s <= o' +. s')

  let mem p = function
    | E -> false
    | R (o, s) -> (o <= p) && (p <= o +. s)

  let equal b b' = b = b'
  let equal_f eq b b' = match b, b' with
    | E, E -> true
    | E, _ | _, E -> false
    | R (o, s), R (o', s') -> eq o o' && eq s s'

  let compare b b' = Pervasives.compare b b'
  let compare_f cmp  b b' = match b, b' with
    | E, E -> 0
    | E, _ -> -1
    | _, E -> 1
    | R (o, s), R (o', s') ->
      let c = cmp o o' in if c <> 0 then c else
        let c = cmp s s' in c

  (* Printers *)

  let _print pp_f ppf b = match b with
    | E -> pp ppf "@[<1>(box1@ empty)@]"
    | R (o, s) ->
      pp ppf "@[<1>(box1 %a@ %a)@]" pp_f o pp_f s

  let pp ppf b = _print (fun ppf f -> Format.fprintf ppf "%g" f) ppf b
  let pp_f pp_f ppf b = _print pp_f ppf b
end

module Box2 = struct
  open V2t
  type t = E | R of p2 * size2
  let dim = 2
  type v = v2
  type p = p2
  type size = size2
  type m = m2

  let err_e () = invalid_arg err_empty_box

  (* Constructors, accessors and constants *)

  let v o s = R (o, s)
  let v_mid m s =
    let o = P2.v (P2.x m -. 0.5 *. Size2.w s) (P2.y m -. 0.5 *. Size2.h s) in
    R (o, s)

  let empty = E
  let o = function E -> err_e () | R (o, _) -> o
  let ox = function E -> err_e () | R (o, _) -> o.x
  let oy = function E -> err_e () | R (o, _) -> o.y
  let size = function E -> err_e () | R (_, size) -> size
  let w = function E -> err_e () | R (_, size) -> size.x
  let h = function E -> err_e () | R (_, size) -> size.y
  let zero = v P2.o Size2.zero
  let unit = v P2.o Size2.unit
  let of_pts p p' =
    let ox, w = if p.x < p'.x then p.x, p'.x -. p.x else p'.x, p.x -. p'.x in
    let oy, h = if p.y < p'.y then p.y, p'.y -. p.y else p'.y, p.y -. p'.y in
    v (P2.v ox oy) (Size2.v w h)

  let add_pt b p = match b with
    | E -> v p Size2.zero
    | R (o, s) as b ->
      let minx = o.x in
      let miny = o.y in
      let maxx = o.x +. s.x in
      let maxy = o.y +. s.y in
      let ox, w =
        if p.x < minx then p.x, maxx -. p.x else
        if p.x > maxx then o.x, p.x -. o.x else
          o.x, s.x
      in
      let oy, h =
        if p.y < miny then p.y, maxy -. p.y else
        if p.y > maxy then o.y, p.y -. o.y else
          o.y, s.y
      in
      if ox = o.x && oy = o.y && w = s.x && h = s.y then b else
        v (P2.v ox oy) (Size2.v w h)

  (* Functions *)

  let min = o
  let minx = ox
  let miny = oy
  let max = function E -> err_e () | R (o, s) -> V2.add o s
  let maxx = function E -> err_e () | R (o, s) -> o.x +. s.x
  let maxy = function E -> err_e () | R (o, s) -> o.y +. s.y
  let mid = function
    | E -> err_e () | R (o, s) -> P2.v (o.x +. 0.5 *. s.x) (o.y +. 0.5 *. s.y)

  let midx = function
    | E -> err_e () | R (o, s) -> o.x +. 0.5 *. s.x

  let midy = function
    | E -> err_e () | R (o, s) -> o.y +. 0.5 *. s.y

  let bl_pt = min
  let bm_pt = function E -> err_e () | R (o, s) -> P2.v (o.x +. 0.5 *. s.x) o.y
  let br_pt = function E -> err_e () | R (o, s) -> P2.v (o.x +. s.x) o.y

  let ml_pt = function E -> err_e () | R (o, s) -> P2.v o.x (o.y +. 0.5 *. s.y)
  let mm_pt = mid
  let mr_pt = function E -> err_e () | R (o, s) -> P2.v (o.x +. s.x)
                                                     (o.y +. 0.5 *. s.y)
  let tl_pt = function E -> err_e () | R (o, s) -> P2.v o.x (o.y +. s.y)
  let tm_pt = function E -> err_e () | R (o, s) -> P2.v (o.x +. 0.5 *. s.x)
                                                     (o.y +. s.y)
  let tr_pt = max

  let area = function E -> 0. | R (_, s) -> s.x *. s.y
  let inter b b' = match b, b' with
    | E, _ | _, E -> E
    | R (o, s), R (o', s') ->
      let l = o.x in let r = l +. s.x in
      let l' = o'.x in let r' = l' +. s'.x in
      if (r < l') || (r' < l) then E else
        let b = o.y in let t = b +. s.y in
        let b' = o'.y in let t' = b' +. s'.y in
        if (t < b') || (t' < b) then E else
          let ox = if l > l' then l else l' in
          let oy = if b > b' then b else b' in
          let w = (if r < r' then r else r') -. ox in
          let h = (if t < t' then t else t') -. oy in
          v (P2.v ox oy) (Size2.v w h)

  let union b b' = match b, b' with
    | E, b | b, E -> b
    | R (o, s), R (o', s') ->
      let ox = if o.x < o'.x then o.x else o'.x in
      let oy = if o.y < o'.y then o.y else o'.y in
      let w =
        let r = o.x +. s.x in let r' = o'.x +. s'.x in
        (if r > r' then r else r') -. ox
      in
      let h =
        let t = o.y +. s.y in let t' = o'.y +. s'.y in
        (if t > t' then t else t') -. oy
      in
      v (P2.v ox oy) (Size2.v w h)

  let inset d = function
    | E -> E
    | R (o, s) ->
      let o' = V2.add o d in
      let w = s.x -. 2. *. d.x in
      let h = s.y -. 2. *. d.y in
      if w < 0. || h < 0. then E else
        v o' (Size2.v w h)

  let round = function
    | E -> E
    | R (o, s) ->
      let ox = floor o.x in
      let oy = floor o.y in
      let w = if (s.x = 0. && ox <> o.x) then 1. else ceil s.x in
      let h = if (s.y = 0. && oy <> o.y) then 1. else ceil s.y in
      v (P2.v ox oy) (Size2.v w h)

  let move d = function E -> E | R (o, s) -> v (V2.add o d) s

  let tr_rect o s tr =
    let r = o.x +. s.x in
    let t = o.y +. s.y in
    let c0 = tr o in
    let c1 = tr (P2.v r o.y) in
    let c2 = tr (P2.v o.x t) in
    let c3 = tr (P2.v r t) in
    let xmin1, xmax1 = if c0.x < c1.x then c0.x, c1.x else c1.x, c0.x in
    let xmin2, xmax2 = if c2.x < c3.x then c2.x, c3.x else c3.x, c2.x in
    let ox = if xmin1 < xmin2 then xmin1 else xmin2 in
    let w = (if xmax1 > xmax2 then xmax1 else xmax2) -. ox in
    let ymin1, ymax1 = if c0.y < c1.y then c0.y, c1.y else c1.y, c0.y in
    let ymin2, ymax2 = if c2.y < c3.y then c2.y, c3.y else c3.y, c2.y in
    let oy = if ymin1 < ymin2 then ymin1 else ymin2 in
    let h = (if ymax1 > ymax2 then ymax1 else ymax2) -. oy in
    v (P2.v ox oy) (Size2.v w h)

  let ltr m = function E -> E | R (o, s) -> tr_rect o s (V2.ltr m)
  let tr m = function E -> E | R (o, s) -> tr_rect o s (P2.tr m)
  let map_f f = function E -> E | R (o, s) -> v (V2.map f o) (V2.map f s)

  (* Predicates and comparisons *)

  let is_empty = function E -> true | R _ -> false
  let is_pt = function E -> false | R (_, s) -> s.x = 0. && s.y = 0.
  let is_seg = function
    | E -> false
    | R (_, s) -> (s.x = 0. && s.y <> 0.) || (s.x <> 0. && s.y = 0.)

  let isects b b' = match b, b' with
    | E, _ | _, E -> false
    | R (o, s), R (o', s') ->
      let l = o.x in let r = l +. s.x in
      let l' = o'.x in let r' = l' +. s'.x in
      if (r < l') || (r' < l) then false else
        let b = o.y in let t = b +. s.y in
        let b' = o'.y in let t' = b' +. s'.y in
        if (t < b') || (t' < b) then false else
          true

  let subset b b' = match b, b' with
    | _b, E -> false
    | E, _b -> true
    | R (o, s), R (o', s') ->
      (o'.x <= o.x) &&
      (o'.y <= o.y) &&
      (o.x +. s.x <= o'.x +. s'.x) &&
      (o.y +. s.y <= o'.y +. s'.y)

  let mem p = function
    | E -> false
    | R (o, s) ->
      (o.x <= p.x) && (p.x <= o.x +. s.x) &&
      (o.y <= p.y) && (p.y <= o.y +. s.y)

  let equal b b' = b = b'
  let equal_f eq b b' = match b, b' with
    | E, E -> true
    | E, _ | _, E -> false
    | R (o, s), R (o', s') -> V2.equal_f eq o o' && V2.equal_f eq s s'

  let compare b b' = Pervasives.compare b b'
  let compare_f cmp  b b' = match b, b' with
    | E, E -> 0
    | E, _ -> -1
    | _, E -> 1
    | R (o, s), R (o', s') ->
      let c = V2.compare_f cmp o o' in if c <> 0 then c else
        let c = V2.compare_f cmp s s' in c

  (* Printers *)

  let _print pp_v2 ppf b = match b with
    | E -> pp ppf "@[<1>(box2@ empty)@]"
    | R (o, s) ->
      pp ppf "@[<1>(box2 %a@ %a)@]" pp_v2 o pp_v2 s

  let pp ppf b = _print V2.pp ppf b
  let pp_f pp_f ppf b = _print (V2.pp_f pp_f) ppf b
end

module Box3 = struct
  open V3t
  type t = E | R of p3 * size3
  let dim = 3
  type v = v3
  type p = p3
  type size = size3
  type m = m3
  let err_e () = invalid_arg err_empty_box

  (* Constructors, accessors and constants *)

  let v o s = R (o, s)
  let v_mid m s =
    let o = P3.v (P3.x m -. 0.5 *. Size3.w s)
        (P3.y m -. 0.5 *. Size3.h s)
        (P3.z m -. 0.5 *. Size3.d s)
    in
    R (o, s)

  let empty = E
  let o = function E -> err_e () | R (o, _) -> o
  let ox = function E -> err_e () | R (o, _) -> o.x
  let oy = function E -> err_e () | R (o, _) -> o.y
  let oz = function E -> err_e () | R (o, _) -> o.z
  let size = function E -> err_e () | R (_, size) -> size
  let w = function E -> err_e () | R (_, size) -> size.x
  let h = function E -> err_e () | R (_, size) -> size.y
  let d = function E -> err_e () | R (_, size) -> size.z
  let zero = v P3.o Size3.zero
  let unit = v P3.o Size3.unit
  let of_pts p p' =
    let ox, w = if p.x < p'.x then p.x, p'.x -. p.x else p'.x, p.x -. p'.x in
    let oy, h = if p.y < p'.y then p.y, p'.y -. p.y else p'.y, p.y -. p'.y in
    let oz, d = if p.z < p'.z then p.z, p'.z -. p.z else p'.z, p.z -. p'.z in
    v (P3.v ox oy oz) (Size3.v w h d)

  let add_pt b p = match b with
    | E -> v p Size3.zero
    | R (o, s) as b ->
      let minx = o.x in
      let miny = o.y in
      let minz = o.z in
      let maxx = o.x +. s.x in
      let maxy = o.y +. s.y in
      let maxz = o.z +. s.z in
      let ox, w =
        if p.x < minx then p.x, maxx -. p.x else
        if p.x > maxx then o.x, p.x -. o.x else
          o.x, s.x
      in
      let oy, h =
        if p.y < miny then p.y, maxy -. p.y else
        if p.y > maxy then o.y, p.y -. o.y else
          o.y, s.y
      in
      let oz, d =
        if p.z < minz then p.z, maxz -. p.z else
        if p.z > maxz then o.z, p.z -. o.z else
          o.z, s.z
      in
      if ox = o.x && oy = o.y && oz = o.z && w = s.x && h = s.y && d = s.z
      then b
      else v (P3.v ox oy oz) (Size3.v w h d)

  (* Functions *)

  let min = o
  let minx = ox
  let miny = oy
  let minz = oz
  let max = function E -> err_e () | R (o, s) -> V3.add o s
  let maxx = function E -> err_e () | R (o, s) -> o.x +. s.x
  let maxy = function E -> err_e () | R (o, s) -> o.y +. s.y
  let maxz = function E -> err_e () | R (o, s) -> o.z +. s.z
  let mid = function
    | E -> err_e () | R (o, s) ->
      P3.v (o.x +. 0.5 *. s.x) (o.y +. 0.5 *. s.y) (o.z +. 0.5 *. s.z)

  let midx = function
    | E -> err_e () | R (o, s) -> o.x +. 0.5 *. s.x

  let midy = function
    | E -> err_e () | R (o, s) -> o.y +. 0.5 *. s.y

  let midz = function
    | E -> err_e () | R (o, s) -> o.z +. 0.5 *. s.z

  let fbl_pt = min
  let fbr_pt = function E -> err_e () | R (o, s) -> P3.v (o.x +. s.x) o.y o.z
  let ftl_pt = function E -> err_e () | R (o, s) -> P3.v o.x (o.y +. s.y) o.z
  let ftr_pt = function E -> err_e () | R (o, s) ->
    P3.v (o.x +. s.x) (o.y +. s.y) o.z

  let nbl_pt = function E -> err_e () | R (o, s) -> P3.v o.x o.y (o.z +. s.z)
  let nbr_pt = function E -> err_e () | R (o, s) ->
    P3.v (o.x +. s.x) o.y (o.z +. s.z)
  let ntl_pt = function E -> err_e () | R (o, s) ->
    P3.v o.x (o.y +. s.y) (o.z +. s.z)
  let ntr_pt = max

  let area = function
    | E -> 0. | R (_, s) -> 2. *. (s.x *. s.y +. s.y *. s.z +. s.z *. s.x)

  let volume = function E -> 0. | R (_, s) -> s.x *. s.y *. s.z
  let inter b b' = match b, b' with
    | E, _ | _, E -> E
    | R (o, s), R (o', s') ->
      let l = o.x in let r = l +. s.x in
      let l' = o'.x in let r' = l' +. s'.x in
      if (r < l') || (r' < l) then E else
        let b = o.y in let t = b +. s.y in
        let b' = o'.y in let t' = b' +. s'.y in
        if (t < b') || (t' < b) then E else
          let n = o.z in let f = n +. s.z in
          let n' = o'.z in let f' = n' +. s'.z in
          if (f < n') || (f' < n) then E else
            let ox = if l > l' then l else l' in
            let oy = if b > b' then b else b' in
            let oz = if n > n' then n else n' in
            let w = (if r < r' then r else r') -. ox in
            let h = (if t < t' then t else t') -. oy in
            let d = (if f < f' then f else f') -. oz in
            v (P3.v ox oy oz) (Size3.v w h d)

  let union b b' = match b, b' with
    | E, b | b, E -> b
    | R (o, s), R (o', s') ->
      let ox = if o.x < o'.x then o.x else o'.x in
      let oy = if o.y < o'.y then o.y else o'.y in
      let oz = if o.z < o'.z then o.z else o'.z in
      let w =
        let r = o.x +. s.x in let r' = o'.x +. s'.x in
        (if r > r' then r else r') -. ox
      in
      let h =
        let t = o.y +. s.y in let t' = o'.y +. s'.y in
        (if t > t' then t else t') -. oy
      in
      let d =
        let f = o.z +. s.z in let f' = o'.z +. s'.z in
        (if f > f' then f else f') -. oz
      in
      v (P3.v ox oy oz) (Size3.v w h d)

  let inset d = function
    | E -> E
    | R (o, s) ->
      let o' = V3.add o d in
      let w = s.x -. 2. *. d.x in
      let h = s.y -. 2. *. d.y in
      let d = s.z -. 2. *. d.z in
      if w < 0. || h < 0. || d < 0. then E else
        v o' (Size3.v w h d)

  let round = function
    | E -> E
    | R (o, s) ->
      let ox = floor o.x in
      let oy = floor o.y in
      let oz = floor o.z in
      let w = if (s.x = 0. && ox <> o.x) then 1. else ceil s.x in
      let h = if (s.y = 0. && oy <> o.y) then 1. else ceil s.y in
      let d = if (s.z = 0. && oz <> o.z) then 1. else ceil s.z in
      v (P3.v ox oy oz) (Size3.v w h d)

  let move d = function E -> E | R (o, s) -> v (V3.add o d) s

  let tr_box o s tr =                           (* that's a little bit ugly. *)
    let r = o.x +. s.x in let t = o.y +. s.y in let f = o.z +. s.z in
    let c0 = tr o in
    let c1 = tr (P3.v o.x o.y f) in
    let c2 = tr (P3.v o.x t o.z) in
    let c3 = tr (P3.v o.x t f) in
    let c4 = tr (P3.v r o.y o.z) in
    let c5 = tr (P3.v r o.y f) in
    let c6 = tr (P3.v r t o.z) in
    let c7 = tr (P3.v r t f) in
    let xmin1, xmax1 = if c0.x < c1.x then c0.x, c1.x else c1.x, c0.x in
    let xmin2, xmax2 = if c2.x < c3.x then c2.x, c3.x else c3.x, c2.x in
    let xmin3, xmax3 = if c4.x < c5.x then c4.x, c5.x else c4.x, c5.x in
    let xmin4, xmax4 = if c6.x < c7.x then c6.x, c7.x else c6.x, c7.x in
    let xmin11 = if xmin1 < xmin2 then xmin1 else xmin2 in
    let xmin12 = if xmin3 < xmin4 then xmin3 else xmin4 in
    let xmax11 = if xmax1 > xmax2 then xmax1 else xmax2 in
    let xmax12 = if xmax3 > xmax4 then xmax3 else xmax4 in
    let ox = if xmin11 < xmin12 then xmin11 else xmin12 in
    let w = (if xmax11 > xmax12 then xmax11 else xmax12) -. ox in
    let ymin1, ymax1 = if c0.y < c1.y then c0.y, c1.y else c1.y, c0.y in
    let ymin2, ymax2 = if c2.y < c3.y then c2.y, c3.y else c3.y, c2.y in
    let ymin3, ymax3 = if c4.y < c5.y then c4.y, c5.y else c4.y, c5.y in
    let ymin4, ymax4 = if c6.y < c7.y then c6.y, c7.y else c6.y, c7.y in
    let ymin11 = if ymin1 < ymin2 then ymin1 else ymin2 in
    let ymin12 = if ymin3 < ymin4 then ymin3 else ymin4 in
    let ymax11 = if ymax1 > ymax2 then ymax1 else ymax2 in
    let ymax12 = if ymax3 > ymax4 then ymax3 else ymax4 in
    let oy = if ymin11 < ymin12 then ymin11 else ymin12 in
    let h = (if ymax11 > ymax12 then ymax11 else ymax12) -. oy in
    let zmin1, zmax1 = if c0.z < c1.z then c0.z, c1.z else c1.z, c0.z in
    let zmin2, zmax2 = if c2.z < c3.z then c2.z, c3.z else c3.z, c2.z in
    let zmin3, zmax3 = if c4.z < c5.z then c4.z, c5.z else c4.z, c5.z in
    let zmin4, zmax4 = if c6.z < c7.z then c6.z, c7.z else c6.z, c7.z in
    let zmin11 = if zmin1 < zmin2 then zmin1 else zmin2 in
    let zmin12 = if zmin3 < zmin4 then zmin3 else zmin4 in
    let zmax11 = if zmax1 > zmax2 then zmax1 else zmax2 in
    let zmax12 = if zmax3 > zmax4 then zmax3 else zmax4 in
    let oz = if zmin11 < zmin12 then zmin11 else zmin12 in
    let d = (if zmax11 > zmax12 then zmax11 else zmax12) -. oz in
    v (P3.v ox oy oz) (Size3.v w h d)

  let ltr m = function E -> E | R (o, s) -> tr_box o s (V3.ltr m)
  let tr m = function E -> E | R (o, s) -> tr_box o s (P3.tr m)
  let map_f f = function E -> E | R (o, s) -> v (V3.map f o) (V3.map f s)

  (* Predicates and comparisons *)

  let is_empty = function E -> true | R _ -> false
  let is_pt = function E -> false | R (_, s) -> s.x = 0. && s.y = 0. && s.z = 0.
  let is_plane = function
    | E -> false
    | R (_, s) ->
      (s.x = 0. && s.y <> 0. && s.z <> 0.) ||
      (s.x <> 0. && s.y = 0. && s.z <> 0.) ||
      (s.x <> 0. && s.y <> 0. && s.z = 0.)

  let is_seg = function
    | E -> false
    | R (_, s) ->
      (s.x = 0. && s.y = 0. && s.z <> 0.) ||
      (s.x = 0. && s.y <> 0. && s.z = 0.) ||
      (s.x <> 0. && s.y = 0. && s.z = 0.)

  let isects b b' = match b, b' with
    | E, _ | _, E -> false
    | R (o, s), R (o', s') ->
      let l = o.x in let r = l +. s.x in
      let l' = o'.x in let r' = l' +. s'.x in
      if (r < l') || (r' < l) then false else
        let b = o.y in let t = b +. s.y in
        let b' = o'.y in let t' = b' +. s'.y in
        if (t < b') || (t' < b) then false else
          let n = o.z in let f = n +. s.z in
          let n' = o'.z in let f' = n' +. s'.z in
          if (f < n') || (f' < n) then false else
            true

  let subset b b' = match b, b' with
    | _b, E -> false
    | E, _b -> true
    | R (o, s), R (o', s') ->
      (o'.x <= o.x) &&
      (o'.y <= o.y) &&
      (o'.z <= o.z) &&
      (o.x +. s.x <= o'.x +. s'.x) &&
      (o.y +. s.y <= o'.y +. s'.y) &&
      (o.z +. s.z <= o'.z +. s'.z)

  let mem p = function
    | E -> false
    | R (o, s) ->
      (o.x <= p.x) && (p.x <= o.x +. s.x) &&
      (o.y <= p.y) && (p.y <= o.y +. s.y) &&
      (o.z <= p.z) && (p.z <= o.z +. s.z)

  let equal b b' = b = b'
  let equal_f eq b b' = match b, b' with
    | E, E -> true
    | E, _ | _, E -> false
    | R (o, s), R (o', s') -> V3.equal_f eq o o' && V3.equal_f eq s s'

  let compare b b' = Pervasives.compare b b'
  let compare_f cmp  b b' = match b, b' with
    | E, E -> 0
    | E, _ -> -1
    | _, E -> 1
    | R (o, s), R (o', s') ->
      let c = V3.compare_f cmp o o' in if c <> 0 then c else
        let c = V3.compare_f cmp s s' in c

  (* Printers *)

  let _print pp_v3 ppf b = match b with
    | E -> pp ppf "@[<1>(box3@ empty)@]"
    | R (o, s) ->
      pp ppf "@[<1>(box3 %a@ %a)@]" pp_v3 o pp_v3 s

  let pp ppf b = _print V3.pp ppf b
  let pp_f pp_f ppf b = _print (V3.pp_f pp_f) ppf b
end

type box1 = Box1.t
type box2 = Box2.t
type box3 = Box3.t

(* Colors *)

type color = V4.t

module Color = struct

  (* Constructors, accessors and constants *)

  type t = color
  type stops = (float * t) list

  let v = V4.v
  let r = V4.x
  let g = V4.y
  let b = V4.z
  let a = V4.w
  let void = v 0. 0. 0. 0.
  let black = v 0. 0. 0. 1.
  let white = v 1. 1. 1. 1.
  let red = v 1. 0. 0. 1.
  let green = v 0. 1. 0. 1.
  let blue = v 0. 0. 1. 1.

  (* Functions *)

  let blend c c' =
    let a = c.V4t.w in
    let a' = c'.V4t.w in
    let mul = (1. -. a) *. a' in
    let a'' = a +. mul in
    if a'' < gg_eps then void else
      v ((a *. c.V4t.x +. mul *. c'.V4t.x) /. a'')
        ((a *. c.V4t.y +. mul *. c'.V4t.y) /. a'')
        ((a *. c.V4t.z +. mul *. c'.V4t.z) /. a'')
        a''

  let clamp c =
    let clamp = ref false in
    let r =
      if c.V4t.x < 0. then (clamp := true; 0.) else
      if c.V4t.x > 1. then (clamp := true; 1.) else c.V4t.x
    in
    let g =
      if c.V4t.y < 0. then (clamp := true; 0.) else
      if c.V4t.y > 1. then (clamp := true; 1.) else c.V4t.y
    in
    let b =
      if c.V4t.z < 0. then (clamp := true; 0.) else
      if c.V4t.z > 1. then (clamp := true; 1.) else c.V4t.z
    in
    let a =
      if c.V4t.w < 0. then (clamp := true; 0.) else
      if c.V4t.w > 1. then (clamp := true; 1.) else c.V4t.w
    in
    if !clamp then v r g b a else c

  let with_a c a = { c with V4t.w = a }

  (* Color conversions *)

  (* sRGB
     N.B. sRGB equations from IEC 61966-2-1:1999, those of the w3c document
     are wrong. *)

  type srgb = v4
  let c0 = 0.04045
  let c1 = 1. /. 12.92
  let c2 = 0.055
  let c3 = 1. /. 1.055
  let c4 = 2.4
  let of_srgb c =                      (* N.B. code duplication with gray. *)
    let r = V4t.(if c.x <= c0 then c1 *. c.x else (c3 *. (c.x +. c2)) ** c4) in
    let g = V4t.(if c.y <= c0 then c1 *. c.y else (c3 *. (c.y +. c2)) ** c4) in
    let b = V4t.(if c.z <= c0 then c1 *. c.z else (c3 *. (c.z +. c2)) ** c4) in
    v r g b c.V4t.w

  let v_srgb ?(a = 1.) r' g' b' =  (* N.B. code duplication with of_srgb. *)
    let r = if r' <= c0 then c1 *. r' else (c3 *. (r' +. c2)) ** c4 in
    let g = if g' <= c0 then c1 *. g' else (c3 *. (g' +. c2)) ** c4 in
    let b = if b' <= c0 then c1 *. b' else (c3 *. (b' +. c2)) ** c4 in
    v r g b a

  let v_srgbi ?a r g b =
    v_srgb ?a (float r /. 255.) (float g /. 255.) (float b /. 255.)

  let gray ?(a = 1.) l' =           (* N.B. code duplication with of_srgb. *)
    let l = if l' <= c0 then c1 *. l' else (c3 *. (l' +. c2)) ** c4 in
    v l l l a

  let c0 = 0.0031308
  let c1 = 12.92
  let c2 = 1.055
  let c3 = 1. /. 2.4
  let c4 = 0.055
  let to_srgb c =
    let r = V4t.(if c.x <= c0 then c1 *. c.x else c2 *. (c.x ** c3) -. c4) in
    let g = V4t.(if c.y <= c0 then c1 *. c.y else c2 *. (c.y ** c3) -. c4) in
    let b = V4t.(if c.z <= c0 then c1 *. c.z else c2 *. (c.z ** c3) -. c4) in
    v r g b c.V4t.w

  let to_srgbi c =              (* N.B. code duplication with [to_srgb]. *)
    let r = V4t.(if c.x <= c0 then c1 *. c.x else c2 *. (c.x ** c3) -. c4) in
    let g = V4t.(if c.y <= c0 then c1 *. c.y else c2 *. (c.y ** c3) -. c4) in
    let b = V4t.(if c.z <= c0 then c1 *. c.z else c2 *. (c.z ** c3) -. c4) in
    (truncate (255. *. r +. 0.5), truncate (255. *. g +. 0.5),
     truncate (255. *. b +. 0.5), c.V4t.w)

  (* CIE Luv *)

  type luv = v4
  let eps = (6. /. 29.) ** 3.
  let eps_inv = 1. /. eps
  let c0 = 1. /. 3.
  let u'n = 0.1978398
  let v'n = 0.4683363
  let _to_luv ~lch c =
    let x = V4t.(0.4124564 *. c.x +.0.3575761 *. c.y +. 0.1804375 *. c.z) in
    let y = V4t.(0.2126729 *. c.x +.0.7151522 *. c.y +. 0.0721750 *. c.z) in
    let z = V4t.(0.0193339 *. c.x +.0.1191920 *. c.y +. 0.9503041 *. c.z) in
    let xyz = x +. 15. *. y +. 3. *. z in
    let u' = 4. *. x /. xyz and v' = 9. *. y /. xyz in
    (* yn = 1.0 *)
    let l = if y > eps then 116. *. (y ** c0) -. 16. else 8. *. eps_inv *. y in
    let l13 = 13. *. l in
    let u = l13 *. (u' -. u'n) and v = l13 *. (v' -. v'n) in
    if not lch then V4.v l u v c.V4t.w else
      let h =
        let h = (atan2 v u) in
        if h < 0. then h +. Float.two_pi else h
      in
      V4.v l (sqrt (u *. u +. v *. v)) h c.V4t.w

  let _of_luv ~lch c =
    let l = c.V4t.x in
    let u = if lch then c.V4t.y *. (cos c.V4t.z) else c.V4t.y in
    let v = if lch then c.V4t.y *. (sin c.V4t.z) else c.V4t.z in
    let l13 = 13. *. l in
    if l13 < gg_eps then V4.v 0. 0. 0. c.V4t.w else
      let u' = u /. l13 +. u'n and v' = v /. l13 +. v'n in
      let y = if l <= 8. then l *. eps /. 8. else ((l +. 16.) /. 116.) ** 3. in
      let x = y *. 9. *. u' /. (4. *. v')
      and z = y *. (12. -. 3. *. u' -. 20. *. v') /. (4. *. v') in
      V4.v
        ( 3.2404548 *. x -. 1.5371389 *. y -. 0.4985315 *. z)
        (-0.9692664 *. x +. 1.8760109 *. y +. 0.0415561 *. z)
        ( 0.0556434 *. x -. 0.2040259 *. y +. 1.0572252 *. z)
        c.V4t.w

  let of_luv c = _of_luv ~lch:false c
  let to_luv c = _to_luv ~lch:false c

  (* CIE L*C*h_uv *)

  type lch_uv = v4
  let of_lch_uv c = _of_luv ~lch:true c
  let to_lch_uv c = _to_luv ~lch:true c

  (* CIE L*a*b* *)

  type lab = v4

  (* The matrix below is XrYrZrD50_of_RGB = scale * XYZD50_of_RGB.
     Compute the XYZD50_of_RGB matrix ourselves:
       D65 = CCT 6504
       D50 = as usual (ICC specified)
       Bradford matrix
       5 fractional digits in the matrix
       scale = M3.scale (V3.div (V3.v 1. 1. 1.) d50)
     Then we match the results from LittleCMS better. *)
  let c0 = 1. /. 3.
  let c1 = 841. /. 108.
  let c2 = 4. /. 29.
  let _to_lab ?(lch = false) c =
    let xr = V4t.(0.4522795 *. c.x +.0.3993744 *. c.y +. 0.1483460 *. c.z) in
    let yr = V4t.(0.2225105 *. c.x +.0.7168863 *. c.y +. 0.0606032 *. c.z) in
    let zr = V4t.(0.0168820 *. c.x +.0.1176865 *. c.y +. 0.8654315 *. c.z) in
    let fx = if xr > eps then xr ** c0 else (c1 *. xr +. c2) in
    let fy = if yr > eps then yr ** c0 else (c1 *. yr +. c2) in
    let fz = if zr > eps then zr ** c0 else (c1 *. zr +. c2) in
    let l = 116. *. fy -. 16. in
    let a = 500. *. (fx -. fy) in
    let b = 200. *. (fy -. fz) in
    if not lch then V4.v l a b c.V4t.w else
      let h =
        let h = atan2 b a in
        if h < 0. then h +. Float.two_pi else h
      in
      V4.v l (sqrt (a *. a +. b *. b)) h c.V4t.w

  (* Matrix below is the inverse of the one above *)
  let eps' = 6. /. 29.
  let c0 = 108. /. 841.
  let c1 = 4. /. 29.
  let _of_lab ?(lch = false) c =
    let l = c.V4t.x in
    let a = if lch then c.V4t.y *. (cos c.V4t.z) else c.V4t.y in
    let b = if lch then c.V4t.y *. (sin c.V4t.z) else c.V4t.z in
    let fy = (l +. 16.) /. 116. in
    let fx = a /. 500. +. fy in
    let fz = fy -. b /. 200. in
    let fx' = if fx > eps' then fx *. fx *. fx else c0 *. (fx -. c1) in
    let fy' = if fy > eps' then fy *. fy *. fy else c0 *. (fy -. c1) in
    let fz' = if fz > eps' then fz *. fz *. fz else c0 *. (fz -. c1) in
    V4.v
      ( 3.0215932 *.fx' -. 1.6168777*.fy' -. 0.4047152 *. fz')
      (-0.9437222 *.fx' +. 1.9161365*.fy' +. 0.0275856 *. fz')
      ( 0.0693906 *.fx' -. 0.2290271*.fy' +. 1.1596365 *. fz')
      c.V4t.w

  let of_lab c = _of_lab ~lch:false c
  let to_lab c = _to_lab ~lch:false c

  (* CIE L*C*h_ab *)

  type lch_ab = v4
  let of_lch_ab c = _of_lab ~lch:true c
  let to_lch_ab c = _to_lab ~lch:true c

  (* Color spaces *)

  type space = [
    | `XYZ | `Lab | `Luv | `YCbr | `Yxy | `RGB | `Gray | `HSV | `HLS
    | `CMYK | `CMY | `CLR2 | `CLR3 | `CLR4 | `CLR5 | `CLR6 | `CLR7
    | `CLR8 | `CLR9 | `CLRA | `CLRB | `CLRC | `CLRD | `CLRE | `CLRF ]

  let space_dim = function
    | `Gray -> 1
    | `CLR2 -> 2
    | `CLR3 | `XYZ | `Lab | `Luv | `YCbr | `Yxy | `RGB | `HSV | `HLS | `CMY -> 3
    | `CLR4 | `CMYK -> 4
    | `CLR5 -> 5 | `CLR6 -> 6 | `CLR7 -> 7 | `CLR8 -> 8 | `CLR9 -> 9
    | `CLRA -> 10 | `CLRB -> 11 | `CLRC -> 12 | `CLRD -> 13 | `CLRE -> 14
    | `CLRF -> 15

  let space_str = function
    | `XYZ -> "XYZ" | `Lab -> "Lab" | `Luv -> "Lub" | `YCbr -> "YCbr"
    | `Yxy -> "Yxy" | `RGB -> "RGB" | `Gray -> "Gray" | `HSV -> "HSV"
    | `HLS -> "HLS" | `CMYK -> "CMYK" | `CMY -> "CMY" | `CLR2 -> "2CLR"
    | `CLR3 -> "3CLR" | `CLR4 -> "4CLR" | `CLR5 -> "5CLR" | `CLR6 -> "6CLR"
    | `CLR7 -> "7CLR" | `CLR8 -> "8CLR" | `CLR9 -> "9CLR" | `CLRA -> "ACLR"
    | `CLRB -> "BCLR" | `CLRC -> "CCLR" | `CLRD -> "DCLR" | `CLRE -> "ECLR"
    | `CLRF -> "FCLR"

  let pp_space ppf s = pp ppf "%s" (space_str s)

  (* Color profiles *)

  type profile = { space : space; icc : string }

  let profile_of_icc icc = try
      let space =
        if String.length icc < 20 then failwith "" else
          match String.sub icc 16 4 with
          | "XYZ " -> `XYZ | "Lab " -> `Lab | "Luv " -> `Luv | "YCbr" -> `YCbr
          | "Yxy " -> `Yxy | "RGB " -> `RGB | "GRAY" -> `Gray | "HSV " -> `HSV
          | "HLS " -> `HLS | "CMYK" -> `CMYK | "CMY " -> `CMY | "2CLR" -> `CLR2
          | "3CLR" -> `CLR3 | "4CLR" -> `CLR4 | "5CLR" -> `CLR5 | "6CLR" -> `CLR6
          | "7CLR" -> `CLR7 | "8CLR" -> `CLR8 | "9CLR" -> `CLR9 | "ACLR" -> `CLRA
          | "BCLR" -> `CLRB | "CCLR" -> `CLRC | "DCLR" -> `CLRD | "ECLR" -> `CLRE
          | "FCLR" -> `CLRF
          | _ -> failwith ""
      in
      Some { space; icc }
    with Failure _ -> None

  let profile_to_icc p = p.icc
  let profile_space p = p.space
  let profile_dim p = space_dim p.space
  let p_gray_l = {
    space = `Gray;
    icc = "\000\000\001`lcms\004 \000\000mntrGRAYXYZ \007\221\000\003\000\012\000\020\000\020\000\023acspAPPL\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\246\214\000\001\000\000\000\000\211-lcms\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004desc\000\000\000\180\000\000\0008cprt\000\000\000\236\000\000\000Nwtpt\000\000\001<\000\000\000\020kTRC\000\000\001P\000\000\000\016mluc\000\000\000\000\000\000\000\001\000\000\000\012enUS\000\000\000\028\000\000\000\028\000g\000r\000a\000y\000 \000b\000u\000i\000l\000t\000-\000i\000n\000\000mluc\000\000\000\000\000\000\000\001\000\000\000\012enUS\000\000\0002\000\000\000\028\000N\000o\000 \000c\000o\000p\000y\000r\000i\000g\000h\000t\000,\000 \000u\000s\000e\000 \000f\000r\000e\000e\000l\000y\000\000\000\000XYZ \000\000\000\000\000\000\246\214\000\001\000\000\000\000\211-para\000\000\000\000\000\000\000\000\000\001\000\000";
  }
  let p_rgb_l = {
    space = `RGB;
    icc = "\000\000\002`lcms\004 \000\000mntrRGB XYZ \007\221\000\003\000\012\000\020\000\t\0006acspAPPL\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\246\214\000\001\000\000\000\000\211-lcms\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011desc\000\000\001\b\000\000\0006cprt\000\000\001@\000\000\000Nwtpt\000\000\001\144\000\000\000\020chad\000\000\001\164\000\000\000,rXYZ\000\000\001\208\000\000\000\020bXYZ\000\000\001\228\000\000\000\020gXYZ\000\000\001\248\000\000\000\020rTRC\000\000\002\012\000\000\000\016gTRC\000\000\002\028\000\000\000\016bTRC\000\000\002,\000\000\000\016chrm\000\000\002<\000\000\000$mluc\000\000\000\000\000\000\000\001\000\000\000\012enUS\000\000\000\026\000\000\000\028\000R\000G\000B\000 \000b\000u\000i\000l\000t\000-\000i\000n\000\000\000\000mluc\000\000\000\000\000\000\000\001\000\000\000\012enUS\000\000\0002\000\000\000\028\000N\000o\000 \000c\000o\000p\000y\000r\000i\000g\000h\000t\000,\000 \000u\000s\000e\000 \000f\000r\000e\000e\000l\000y\000\000\000\000XYZ \000\000\000\000\000\000\246\214\000\001\000\000\000\000\211-sf32\000\000\000\000\000\000\244\149\255\255\250\019\000\000\016+\255\255\248\183\000\001\002\150\000\000\005a\000\000\003%\255\255\250\196\000\001TgXYZ \000\000\000\000\000\000o\148\000\0008\238\000\000\003\144XYZ \000\000\000\000\000\000$\157\000\000\015\131\000\000\182\190XYZ \000\000\000\000\000\000b\165\000\000\183\144\000\000\024\222para\000\000\000\000\000\000\000\000\000\001\000\000para\000\000\000\000\000\000\000\000\000\001\000\000para\000\000\000\000\000\000\000\000\000\001\000\000chrm\000\000\000\000\000\003\000\000\000\000\163\215\000\000T{\000\000L\205\000\000\153\154\000\000&f\000\000\015\\";
  }
end

(* Linear bigarrays and bigarray buffers *)

type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

type buffer =
  [ `Int8 of (int, Bigarray.int8_signed_elt) bigarray
  | `Int16 of (int, Bigarray.int16_signed_elt) bigarray
  | `Int32 of (int32, Bigarray.int32_elt) bigarray
  | `Int64 of (int64, Bigarray.int64_elt) bigarray
  | `UInt8 of (int, Bigarray.int8_unsigned_elt) bigarray
  | `UInt16 of (int, Bigarray.int16_unsigned_elt) bigarray
  | `UInt32 of (int32, Bigarray.int32_elt) bigarray
  | `UInt64 of (int64, Bigarray.int64_elt) bigarray
  | `Float16 of (int, Bigarray.int16_unsigned_elt) bigarray
  | `Float32 of (float, Bigarray.float32_elt) bigarray
  | `Float64 of (float, Bigarray.float64_elt) bigarray ]

module Ba = struct

  (* Scalar types *)

  type ('a, 'b) ba_scalar_type =
    | Int8 : (int, Bigarray.int8_signed_elt) ba_scalar_type
    | Int16 : (int, Bigarray.int16_signed_elt) ba_scalar_type
    | Int32 : (int32, Bigarray.int32_elt) ba_scalar_type
    | Int64 : (int64, Bigarray.int64_elt) ba_scalar_type
    | UInt8 : (int, Bigarray.int8_unsigned_elt) ba_scalar_type
    | UInt16 : (int, Bigarray.int16_unsigned_elt) ba_scalar_type
    | UInt32 : (int32, Bigarray.int32_elt) ba_scalar_type
    | UInt64 : (int64, Bigarray.int64_elt) ba_scalar_type
    | Float16 : (int, Bigarray.int16_unsigned_elt) ba_scalar_type
    | Float32 : (float, Bigarray.float32_elt) ba_scalar_type
    | Float64 : (float, Bigarray.float64_elt) ba_scalar_type

  let ba_kind_of_ba_scalar_type
    : type a b. (a, b) ba_scalar_type -> (a, b) Bigarray.kind
    = function
      | Int8 -> Bigarray.int8_signed
      | Int16 -> Bigarray.int16_signed
      | Int32 -> Bigarray.int32
      | Int64 -> Bigarray.int64
      | UInt8 -> Bigarray.int8_unsigned
      | UInt16 -> Bigarray.int16_unsigned
      | UInt32 -> Bigarray.int32
      | UInt64 -> Bigarray.int64
      | Float16 -> Bigarray.int16_unsigned
      | Float32 -> Bigarray.float32
      | Float64 -> Bigarray.float64

  type scalar_type =
    [ `Int8 | `Int16 | `Int32 | `Int64 | `UInt8 | `UInt16 | `UInt32 | `UInt64
    | `Float16 | `Float32 | `Float64 ]

  let scalar_type_of_ba_scalar_type :
    type a b. (a, b) ba_scalar_type -> scalar_type = function
    | Int8 -> `Int8 | Int16 -> `Int16 | Int32 -> `Int32 | Int64 -> `Int64
    | UInt8 -> `UInt8 | UInt16 -> `UInt16 | UInt32 -> `UInt32
    | UInt64 -> `UInt64 | Float16 -> `Float16 | Float32 -> `Float32
    | Float64 -> `Float64

  let scalar_type_byte_count = function
    | `Int8 | `UInt8 -> 1
    | `Int16 | `UInt16 | `Float16 -> 2
    | `Int32 | `UInt32 | `Float32 -> 4
    | `Int64 | `UInt64 | `Float64 -> 8

  let pp_scalar_type ppf st = pp ppf begin match st with
      | `Int8 -> "int8" | `Int16 -> "int16" | `Int32 -> "int32"
      | `Int64 -> "int64" | `UInt8 -> "uint8" | `UInt16 -> "uint16"
      | `UInt32 -> "uInt32" | `UInt64 -> "uint64" | `Float16 -> "float16"
      | `Float32 -> "float32" | `Float64 -> "float64"
    end

  (* Bigarray buffers. *)

  let ba_create st count =
    let kind = ba_kind_of_ba_scalar_type st in
    Bigarray.Array1.create kind Bigarray.c_layout count

  module Buffer = struct
    type t = buffer

    let create st count = match st with
      | `Int8 -> `Int8 (ba_create Int8 count)
      | `Int16 -> `Int16 (ba_create Int16 count)
      | `Int32 -> `Int32 (ba_create Int32 count)
      | `Int64 -> `Int64 (ba_create Int64 count)
      | `UInt8 -> `UInt8 (ba_create UInt8 count)
      | `UInt16 -> `UInt16 (ba_create UInt16 count)
      | `UInt32 -> `UInt32 (ba_create UInt32 count)
      | `UInt64 -> `UInt64 (ba_create UInt64 count)
      | `Float16 -> `Float16 (ba_create Float16 count)
      | `Float32 -> `Float32 (ba_create Float32 count)
      | `Float64 -> `Float64 (ba_create Float64 count)

    let scalar_type = function
      | `Int8 _ -> `Int8 | `Int16 _ -> `Int16 | `Int32 _ -> `Int32
      | `Int64 _ -> `Int64 | `UInt8 _ -> `UInt8 | `UInt16 _ -> `UInt16
      | `UInt32 _ -> `UInt32 | `UInt64 _ -> `UInt64 | `Float16 _ -> `Float16
      | `Float32 _ -> `Float32 | `Float64 _ -> `Float64

    let length_units ~bytes (b : buffer) = match b with
      | `Int8 b -> Bigarray.Array1.dim b
      | `Int16 b -> Bigarray.Array1.dim b * (if bytes then 2 else 1)
      | `Int32 b -> Bigarray.Array1.dim b * (if bytes then 4 else 1)
      | `Int64 b -> Bigarray.Array1.dim b * (if bytes then 8 else 1)
      | `UInt8 b -> Bigarray.Array1.dim b
      | `UInt16 b -> Bigarray.Array1.dim b * (if bytes then 2 else 1)
      | `UInt32 b -> Bigarray.Array1.dim b * (if bytes then 4 else 1)
      | `UInt64 b -> Bigarray.Array1.dim b * (if bytes then 8 else 1)
      | `Float16 b -> Bigarray.Array1.dim b * (if bytes then 2 else 1)
      | `Float32 b -> Bigarray.Array1.dim b * (if bytes then 4 else 1)
      | `Float64 b -> Bigarray.Array1.dim b * (if bytes then 8 else 1)

    let length b = length_units ~bytes:false b
    let byte_length b = length_units ~bytes:true b

    (* FIXME: It will be possible to remove the Obj.magics once we
       have GADTs for bigarray kinds.
       See http://caml.inria.fr/mantis/view.php?id=6064 *)

    let of_bigarray ?data ba : buffer =
      let err_buffer_data data k =
        let str_of_data = function
          | None -> assert false
          | Some `Float -> "`Float"
          | Some `Unsigned -> "`Unsigned"
        in
        let st = str "%a" pp_scalar_type k in
        invalid_arg (err_buffer_data (str_of_data data) st)
      in
      let check_data_none d k = if d <> None then err_buffer_data d k in
      let open Bigarray in
      match Obj.magic (Bigarray.Array1.kind ba) with
      | k when k = int8_signed ->
        check_data_none data `Int8; `Int8 (Obj.magic ba)
      | k when k = int16_signed ->
        check_data_none data `Int16; `Int16 (Obj.magic ba)
      | k when k = int32 ->
        begin match data with
          | None -> `Int32 (Obj.magic ba)
          | Some `Unsigned -> `UInt32 (Obj.magic ba)
          | Some _ -> err_buffer_data data `Int32
        end
      | k when k = int64 ->
        begin match data with
          | None -> `Int64 (Obj.magic ba)
          | Some `Unsigned -> `UInt64 (Obj.magic ba)
          | Some _ -> err_buffer_data data `Int64
        end
      | k when k = int8_unsigned ->
        check_data_none data `UInt8; `UInt8 (Obj.magic ba)
      | k when k = int16_unsigned ->
        begin match data with
          | None -> `UInt16 (Obj.magic ba)
          | Some `Float -> `Float16 (Obj.magic ba)
          | Some _ -> err_buffer_data data `Int16
        end
      | k when k = float32 ->
        check_data_none data `Float32; `Float32 (Obj.magic ba)
      | k when k = float64 ->
        check_data_none data `Float64; `Float64 (Obj.magic ba)
      | _ -> invalid_arg err_buffer_kind

    let pp ppf b =
      pp ppf "@[<1>(buffer@ %a %d)@]" pp_scalar_type (scalar_type b) (length b)
  end

  (* Linear bigarrays *)

  let create = ba_create
  let unsafe_get = Bigarray.Array1.unsafe_get
  let unsafe_set = Bigarray.Array1.unsafe_set

  let length b = Bigarray.Array1.dim b
  let sub = Bigarray.Array1.sub
  let blit src si dst di len =
    let src = if si = 0 && length src = len then src else sub src si len in
    let dst = if di = 0 && length dst = len then dst else sub dst di len in
    Bigarray.Array1.blit src dst

  let fill = Bigarray.Array1.fill
  let of_array st a =
    let kind = ba_kind_of_ba_scalar_type st in
    Bigarray.Array1.of_array kind Bigarray.c_layout a

  let of_list st l =
    let ba = create st (List.length l) in
    List.iteri (unsafe_set ba) l;
    ba

  let of_bytes (type a) (type b) (k : (a, b) ba_scalar_type) s :
    (a, b) bigarray =
    match k with
    | Int8 ->
      let b = create Int8 (String.length s) in
      for i = 0 to String.length s - 1 do
        b.{i} <- Char.code (String.unsafe_get s i)
      done;
      b
    | UInt8 ->
      let b = create UInt8 (String.length s) in
      for i = 0 to String.length s - 1 do
        let v = Char.code (String.unsafe_get s i) in
        b.{i} <- v - (v lsr 7 * 0x100)
      done;
      b
    | _ -> (* TODO *) invalid_arg "unsupported bigarray scalar type"

  let pp ?count ?stride ?(first = 0) ?(dim = 1) ~pp_scalar ppf ba =
    let pp = pp in
    let ba_len = length ba in
    let stride = match stride with None -> dim | Some stride -> stride in
    let count = match count with
      | Some count -> count
      | None -> (ba_len + (stride - dim) - first) / stride
    in
    if first + count * stride >= ba_len
    then invalid_arg (err_pp_ba_spec ~first ~stride ~count ~len:ba_len)
    else
      let i = ref first in
      let pp_group ppf () =
        pp ppf "@[<1>(%a" pp_scalar ba.{!i};
        for c = 1 to dim - 1 do pp ppf "@ %a" pp_scalar ba.{!i + c} done;
        pp ppf ")@]";
        i := !i + stride
      in
      pp ppf "@[<hov>%a" pp_group ();
      for _k = 1 to count - 1 do pp ppf "@ %a" pp_group (); done;
      pp ppf "@]"

  (* Get *)

  let get_v2 b i = V2.v b.{i} b.{i+1}
  let get_v3 b i = V3.v b.{i} b.{i+1} b.{i+2}
  let get_v4 b i = V4.v b.{i} b.{i+1} b.{i+2} b.{i+3}

  let get_2d b i = b.{i}, b.{i+1}
  let get_3d b i = b.{i}, b.{i+1}, b.{i+2}
  let get_4d b i = b.{i}, b.{i+1}, b.{i+2}, b.{i+3}

  let ic = Int32.to_int
  let geti_2d b i = ic b.{i}, ic b.{i+1}
  let geti_3d b i = ic b.{i}, ic b.{i+1}, ic b.{i+2}

  (* Set *)

  let set_v2 b i v = b.{i} <- V2.x v; b.{i+1} <- V2.y v
  let set_v3 b i v = b.{i} <- V3.x v; b.{i+1} <- V3.y v; b.{i+2} <- V3.z v
  let set_v4 b i v =
    b.{i} <- V4.x v; b.{i+1} <- V4.y v; b.{i+2} <- V4.z v; b.{i+3} <- V4.w v

  let set_2d b i x y = b.{i} <- x; b.{i+1} <- y
  let set_3d b i x y z = b.{i} <- x; b.{i+1} <- y; b.{i+2} <- z
  let set_4d b i x y z w =
    b.{i} <- x; b.{i+1} <- y; b.{i+2} <- z; b.{i+3} <- w

  let ic = Int32.of_int
  let seti_2d b i x y = b.{i} <- ic x; b.{i+1} <- ic y
  let seti_3d b i x y z = b.{i} <- ic x; b.{i+1} <- ic y; b.{i+2} <- ic z
end

(* Raster data *)

module Raster = struct

  (* Argument validators *)

  let check_first v = if v >= 0 then () else invalid_arg (err_rfirst v)
  let check_index a v = if v >= 1. then () else invalid_arg (err_rindex a v)
  let check_stride a v min =
    let min = int_of_float min in
    if v >= min then () else invalid_arg (err_rstride a v min)

  let check_range k a v min max =
    if min <= v && v <= max then () else
      invalid_arg (err_rrange k a v min max)

  let check_sub_pos = check_range "pos"
  let check_sub_index = check_range "index"

  (* Samples *)

  module Sample = struct

    (* Sample semantics *)

    type semantics = [ `Color of Color.profile * bool | `Other of string * int ]

    let rgb_l = `Color (Color.p_rgb_l, false)
    let rgba_l = `Color (Color.p_rgb_l, true)
    let gray_l = `Color (Color.p_gray_l, false)
    let graya_l = `Color (Color.p_gray_l, true)
    let pp_semantics ppf = function
      | `Color (p, a) ->
        let a = if a then "A" else "" in
        pp ppf "%a%s" Color.pp_space (Color.profile_space p) a
      | `Other (label, d) ->
        pp ppf "%s(%dD)" label d

    (* Sample format *)

    type pack =
      [ `PU8888 | `FourCC of string * Ba.scalar_type option
      | `Other of string * Ba.scalar_type option ]

    let pp_pack ppf = function
      | `PU8888 -> pp ppf "P8888"
      | `FourCC (c, _) -> pp ppf "'%s'" c
      | `Other (s, _) -> pp ppf "%s" s

    type format =
      { semantics : semantics;
        scalar_type : Ba.scalar_type;
        pack : pack option; }

    let format ?pack semantics scalar_type = match pack with
      | None -> { semantics; scalar_type; pack }
      | Some p ->
        let restrict = match p with
          | `PU8888 -> Some `UInt64
          | `Other (_, r) -> r
          | `FourCC (c, r) ->
            if String.length c = 4 then r else
              invalid_arg (err_illegal_fourcc c)
        in
        match restrict with
        | None -> { semantics; scalar_type; pack }
        | Some st ->
          if st = scalar_type then { semantics; scalar_type; pack } else
            invalid_arg
              (err_sample_pack
                 (str "%a" pp_pack p) (str "%a" Ba.pp_scalar_type scalar_type))

    let semantics sf = sf.semantics
    let scalar_type sf = sf.scalar_type
    let pack sf = sf.pack
    let dim sf = match sf.semantics with
      | `Other (_label, dim) -> dim
      | `Color (profile, alpha) ->
        Color.profile_dim profile + (if alpha then 1 else 0)

    let scalar_count ?(first = 0) ?w_stride ?h_stride size sf =
      let w, h, d = match size with
        | `D1 w -> Float.round w, 1., 1.
        | `D2 s ->
          Float.round (Size2.w s),
          Float.round (Size2.h s),
          1.
        | `D3 s ->
          Float.round (Size3.w s),
          Float.round (Size3.h s),
          Float.round (Size3.d s)
      in
      let w_stride = match w_stride with None -> int_of_float w | Some s -> s in
      let h_stride = match h_stride with None -> int_of_float h | Some s -> s in
      check_first first;
      check_index "width" w; check_index "height" h ; check_index "depth" d;
      check_stride "w" w_stride w; check_stride "h" h_stride h;
      let x_stride = dim sf in
      let y_stride = x_stride * w_stride in
      let z_stride = y_stride * h_stride in
      let size = z_stride * int_of_float d in
      first + size

    let pp_format ppf sf =
      let pp_opt_pack ppf op = match op with
        | None -> () | Some pack -> pp ppf "@ %a" pp_pack pack
      in
      pp ppf "@[<1>(raster-sf@ %a@ %a%a)@]"
        pp_semantics sf.semantics Ba.pp_scalar_type sf.scalar_type
        pp_opt_pack sf.pack
  end

  type t =
    { res : v3 option;
      first : int; w_stride : int; h_stride : int;
      size : size3;
      sf : Sample.format;
      buf : buffer;  }

  let v ?res ?(first = 0) ?w_stride ?h_stride size sf buf =
    let w, h, d = match size with
      | `D1 w -> Float.round w, 1., 1.
      | `D2 s ->
        Float.round (Size2.w s),
        Float.round (Size2.h s),
        1.
      | `D3 s ->
        Float.round (Size3.w s),
        Float.round (Size3.h s),
        Float.round (Size3.d s)
    in
    let size = Size3.v w h d in
    let w_stride = match w_stride with None -> int_of_float w | Some s -> s in
    let h_stride = match h_stride with None -> int_of_float h | Some s -> s in
    check_first first;
    check_index "width" w; check_index "height" h ; check_index "depth" d;
    check_stride "w" w_stride w; check_stride "h" h_stride h;
    { res; first; w_stride; h_stride; size; sf; buf}

  let res_default = 11811.
  let res r = r.res
  let get_res r = match r.res with
    | None -> invalid_arg err_rresnone
    | Some r -> r

  let first r = r.first
  let w_stride r = r.w_stride
  let h_stride r = r.h_stride
  let sample_format r = r.sf
  let buffer r = r.buf

  let extent axis ~meters r =
    if not meters then axis r.size else
      let res = match r.res with None -> res_default | Some r -> axis r in
      axis r.size /. res

  let wi r = int_of_float (Size3.w r.size)
  let hi r = int_of_float (Size3.h r.size)
  let di r = int_of_float (Size3.d r.size)
  let w ?(meters = false) r = extent Size3.w ~meters r
  let h ?(meters = false) r = extent Size3.h ~meters r
  let d ?(meters = false) r = extent Size3.d ~meters r

  let size1 = w
  let size2 ?(meters = false) r =
    if not meters then V2.of_v3 r.size else
      let wres, hres = match r.res with
        | None -> res_default, res_default
        | Some res -> V3.x res, V3.y res
      in
      Size2.v (Size3.w r.size /. wres ) (Size3.h r.size /. hres)

  let size3 ?(meters = false) r =
    if not meters then r.size else
      let wres, hres, dres = match r.res with
        | None -> res_default, res_default, res_default
        | Some res -> V3.x res, V3.y res, V3.z res
      in Size3.v
        (Size3.w r.size /. wres)
        (Size3.h r.size /. hres)
        (Size3.d r.size /. dres)

  let box1 ?meters ?(mid = false) ?(o = 0.) r =
    if mid then Box1.v_mid o (size1 ?meters r) else
      Box1.v o (size1 ?meters r)

  let box2 ?meters ?(mid = false) ?(o = P2.o) r =
    if mid then Box2.v_mid o (size2 ?meters r) else
      Box2.v o (size2 ?meters r)

  let box3 ?meters ?(mid = false) ?(o = P3.o) r =
    if mid then Box3.v_mid P3.o (size3 ?meters r) else
      Box3.v o (size3 ?meters r)

  let dim r =
    if Size3.d r.size > 1. then 3 else
    if Size3.h r.size > 1. then 2 else 1

  let kind r = match dim r with
    | 1 -> `D1 | 2 -> `D2 | 3 -> `D3 | _ -> assert false

  let scalar_strides r =
    if r.sf.Sample.pack <> None then invalid_arg err_packed_sf;
    let x_stride = Sample.dim r.sf in
    let y_stride = x_stride * r.w_stride in
    let z_stride = y_stride * r.h_stride in
    x_stride, y_stride, z_stride

  let sub box r =
    if r.sf.Sample.pack <> None then invalid_arg err_packed_sf;
    let round = Float.round in
    let x, y, z, w, h, d = match box with
      | `D1 b ->
        round (Box1.ox b), 0., 0.,
        round (Box1.w b), 1., 1.
      | `D2 b ->
        round (Box2.ox b), round (Box2.oy b), 0.,
        round (Box2.w b), round (Box2.h b), 1.
      | `D3 b ->
        round (Box3.ox b), round (Box3.oy b), round (Box3.oz b),
        round (Box3.w b), round (Box3.d b), round (Box3.h b)
    in
    let size = Size3.v w h d in
    let x, y, z = int_of_float x, int_of_float y, int_of_float z in
    let w, h, d = int_of_float w, int_of_float h, int_of_float d in
    let rw, rh, rd = int_of_float (Size3.w r.size),
                     int_of_float (Size3.h r.size),
                     int_of_float (Size3.d r.size)
    in
    check_sub_pos "x" x 0 (rw - 1);
    check_sub_pos "y" y 0 (rh - 1);
    check_sub_pos "z" z 0 (rd - 1);
    check_sub_index "width" w 1 rw;
    check_sub_index "height" h 1 rh;
    check_sub_index "depth" d 1 rd;
    let x_stride, y_stride, z_stride = scalar_strides r in
    let first' = r.first + z * z_stride + y * y_stride + x * x_stride in
    let w_stride' = r.w_stride (* + (rw - w) *) in
    let h_stride' = r.h_stride (* + (rh - h) *) in
    { res = r.res; first = first'; w_stride = w_stride'; h_stride = h_stride';
      size; sf = r.sf; buf = r.buf }

  let equal r r' = r = r'
  let compare r r' = Pervasives.compare r r'
  let pp ppf r =
    pp ppf "@[<1>(raster@ %a@ %a@ %a)@]"
      V3.pp (size3 r) Sample.pp_format r.sf Ba.Buffer.pp r.buf

  let inch_to_meter = 0.0254
  let spm_of_spi spi = spi /. inch_to_meter
  let spm_to_spi spm = spm *. inch_to_meter
end

type raster = Raster.t

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The gg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

