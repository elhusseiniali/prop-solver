(** Type variables. *)
type tvar = string
(** Term variables. *)
type var = string


type ty =
    | Term of tvar
    | Impl of (ty * ty)


(**
    Lambda terms à la Church:
    t, u ::= x | tu | Lambda.x^A.t,
    where t, u: var,
          A: ty (simple type)

 **)
type tm =
    | Var of var
    | App of (var * var)
    | Abs of (var * ty * var)


let rec string_of_ty e =
  match e with
    | Term(t) -> t
    | Impl(t1, t2) -> String.concat "" ["("; String.concat " \u{21d2} " [string_of_ty(t1); string_of_ty(t2)]; ")"]


let x1:tvar = "x1";;
let x2:tvar = "x2";;


let xt1:ty = Term(x1);;
let xt2:ty = Term(x2);;
let xt:ty = Impl(xt1, xt2);;
let xt3:ty = Impl(xt1, xt);;


let () =
    print_endline (string_of_ty(xt));;
    print_endline (string_of_ty(xt3))

