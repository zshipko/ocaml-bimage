open Bimage
open Expr

exception Not_implemented of string

let rec to_z3: type a. Z3.context -> a t -> Z3.Expr.expr = fun ctx expr ->
  match expr with
  | Kernel _k -> Z3.FloatingPoint.mk_const_s ctx "Kernel" (Z3.FloatingPoint.mk_sort_64 ctx)
  | Input _ -> Z3.FloatingPoint.mk_const_s ctx "Kernel" (Z3.FloatingPoint.mk_sort_64 ctx)
  | X -> Z3.Arithmetic.Integer.mk_const_s ctx "X"
  | Y -> Z3.Arithmetic.Integer.mk_const_s ctx "Y"
  | C -> Z3.Arithmetic.Integer.mk_const_s ctx "C"
  | Int i -> Z3.Arithmetic.Integer.mk_numeral_i ctx i
  | Float f -> Z3.FloatingPoint.mk_numeral_f ctx f (Z3.FloatingPoint.mk_sort_64 ctx)
  | Float_of_int _ as i ->
      let a = compile (ref 0) (ref 0) (ref 0) i [||] in
      Z3.FloatingPoint.mk_numeral_f ctx a (Z3.FloatingPoint.mk_sort_64 ctx)
  | Int_of_float _ as i ->
      let a = compile (ref 0) (ref 0) (ref 0) i [||] in
      Z3.Arithmetic.Integer.mk_numeral_i ctx a
  | Fadd (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_add ctx [a; b]
  | Fsub (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_sub ctx [a; b]
  | Fmul (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_mul ctx [a; b]
  | Fdiv (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_div ctx a b
  | Fpow (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_power ctx a b
  | Fsqrt a ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx (Float 0.5) in
      Z3.Arithmetic.mk_power ctx a b
  | Iadd (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_add ctx [a; b]
  | Isub (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_sub ctx [a; b]
  | Imul (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_mul ctx [a; b]
  | Idiv (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_div ctx a b
  | Gt (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_gt ctx a b
  | Lt (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.mk_lt ctx a b
  | Eq (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      let a' = Z3.Arithmetic.mk_ge ctx a b in
      let b' = Z3.Arithmetic.mk_le ctx a b in
      Z3.Boolean.mk_and ctx [a'; b']
  | Bool true ->
      Z3.Boolean.mk_true ctx
  | Bool false ->
      Z3.Boolean.mk_false ctx
  | And (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Boolean.mk_and ctx [a; b]
  | Or (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Boolean.mk_or ctx [a; b]

  | Imod (a, b) ->
      let a = to_z3 ctx a in
      let b = to_z3 ctx b in
      Z3.Arithmetic.Integer.mk_mod ctx a b
  | Not b ->
      let b = to_z3 ctx b in
      Z3.Boolean.mk_not ctx b
  | If _ -> raise (Not_implemented "If")
  | Fmod _ -> raise (Not_implemented "Fmod")
  | Fsin _ ->  raise (Not_implemented "Fsin")
  | Fcos _ -> raise (Not_implemented  "Fcos")
  | Ftan _ -> raise (Not_implemented  "Ftan")
