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
    | Abs of (var * ty * tm)


let rec string_of_ty e =
  match e with
    | Term(t) -> t
    | Impl(t1, t2) -> String.concat "" ["("; String.concat " \u{21d2} " [string_of_ty(t1); string_of_ty(t2)]; ")"]



let rec string_of_tm e =
  match e with
  | Var(t) -> t
  | App(a, b) -> String.concat "" ["("; String.concat " " [a; b]; ")"]
  | Abs(a, t, b) -> String.concat "" ["\u{03bb}"; "("; a; ": "; string_of_ty(t); ")."; string_of_tm(b)]



let a:tvar = "A";;
let b:tvar = "B";;
let c:tvar = "C";;


let at:ty = Term(a);;
let bt:ty = Term(b);;
let ct:ty = Term(c);;

let abt:ty = Impl(at, bt);;
let act:ty = Impl(at, ct);;
let abact:ty = Impl(abt, act);;


let x:var = "x";;
let f:var = "f";;


let xvar:tm = Var(x);;
let fx:tm = App(f, x);;
let xf:tm = Abs(x, at, fx);;

let lt1:tm = Abs(f, abt, fx);;
let lt2:tm = Abs(f, abt, xf);;


let () =
    print_endline (string_of_ty(abact));;
    print_endline (string_of_tm(fx));;
    print_endline (string_of_tm(lt1));;
    print_endline (string_of_tm(lt2));;
