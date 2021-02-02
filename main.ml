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

 *)
type tm =
    | Var of var
    | App of (var * var)
    | Abs of (var * ty * tm)


(**
    1.3.
 *)
(**
    Return a string representation for Simple Types.
 *)
let rec string_of_ty e =
  match e with
    | Term(t) -> t
    | Impl(t1, t2) -> String.concat "" ["("; String.concat " \u{21d2} " [string_of_ty(t1); string_of_ty(t2)]; ")"]



(**
    Return a string representation for Lambda-terms.
 *)
let rec string_of_tm e =
  match e with
  | Var(t) -> t
  | App(a, b) -> String.concat "" ["("; String.concat " " [a; b]; ")"]
  | Abs(a, t, b) -> String.concat "" ["\u{03bb}"; "("; a; ": "; string_of_ty(t); ")."; string_of_tm(b)]



(**
    1.4.
 *)
type context = (string * ty) list
exception Type_error

let eelse:tvar = "Else";;

let rec infer_type c t =
    try
        match t with
        | Var(e) -> List.assoc e c
        | App(a, b) -> (
            match List.assoc a c with
             | Term(x) -> raise Type_error
             | Impl(x, y) -> (
                match List.assoc b c with
                  | Term(x) -> y
                  | Impl(_, _) -> raise Type_error
                )
            )
        | Abs(a, tp, b) -> (
            Impl(tp, infer_type (c) (b))
        )
    with
        | Not_found -> raise Type_error


(**A→B
    Trying things out.
 *)
let a:tvar = "A";;
let b:tvar = "B";;
let c:tvar = "C";;


let at:ty = Term a;;
let bt:ty = Term b;;
let ct:ty = Term c;;

let abt:ty = Impl(at, bt);;
let act:ty = Impl(at, ct);;
let abact:ty = Impl(abt, act);;


let x:var = "x";;
let f:var = "f";;
let t:var = "t";;


let xvar:tm = Var(x);;
let tvar:tm = Var(t);;

let fx:tm = App(f, x);;
let xf:tm = Abs(x, at, fx);;

let lt1:tm = Abs(f, abt, fx);;
let lt2:tm = Abs(f, abt, xf);;

let av:var = "a"
let ca:context = [(av, at)]
let avar:tm = Var(av)

let cab:context = [(b, abt); (a, at)]
let ab:tm = App(b, a)

let cxt:context = [(x, at); (t, bt)]
let xt:tm = Abs(x, at, tvar)



let () =
    print_endline (string_of_ty(abact));;
    print_endline (string_of_tm(fx));;
    print_endline (string_of_tm(lt1));;
    print_endline (string_of_tm(lt2));;
    print_string (av ^ ":" ^ string_of_ty(at) ^ ": ");;
    print_endline (string_of_ty(infer_type (ca) (avar)));;
    print_string (b ^ ":" ^string_of_ty(abt) ^ ".");;
    print_string (a ^ ":" ^ string_of_ty(at) ^ ": ");;
    print_endline (string_of_ty(infer_type (cab) (ab)));;
    print_endline (string_of_ty(infer_type (cxt) (xt)));;
