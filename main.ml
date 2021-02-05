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
    | App of (tm * tm)
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
    | App(a, b) -> String.concat "" ["("; String.concat "." [string_of_tm a; string_of_tm b]; ")"]
    | Abs(a, t, b) -> String.concat "" ["\u{03bb}"; "("; a; ": "; string_of_ty(t); ")."; string_of_tm(b)]



(**
    1.4.
 *)
type context = (string * ty) list
exception Type_error

let ex1 = Failure "first"
let ex2 = Failure "second"


let rec old_infer_type c t =
    try
        match t with
            | Var(e) -> List.assoc e c
            | App(b, a) -> (
                match b with
                    | Var(b1) -> (
                        match List.assoc b1 c with
                            | Term(x) -> raise Type_error
                            (** Raise Type_error because a Term Type does not
                            represent a function *)
                            | Impl(x, y) -> (
                                if x <> old_infer_type c a
                                then raise Type_error
                                else y
                            )
                    )
                    | _ -> raise Type_error
                    (** I think we don't reach this because of left-associativity *)
                )
            | Abs(a, tp, b) -> (
                Impl(tp, old_infer_type ((a, tp)::c) (b))
            )
    with
        | Not_found -> raise Type_error


(**
    1.6. Type inference and type checking as mutually
    recursive functions.

I initially started with modifying old_infer_type to write this, then
I noticed that a (shorter) implementation was given in the book.
I left my infer_type as-is (renamed to old_infer_type)
as some evidence that I actually worked on
it on my own; the more "advanced" syntax comes from the book;
you can tell that I am an OCaml novice when you compare the earlier function
definition to the following one.

I made sure that infer_type and old_infer_type produce the same results for the
test cases and a few extra examples.
 *)
let rec infer_type env = function
    | Var x ->
        (try List.assoc x env with Not_found -> raise Type_error)
    | Abs (x, a, t) ->
        Impl(a, infer_type ((x, a)::env) t)
    | App (t, u) ->
        match infer_type env t with
            | Impl(a, b) -> if check_type env u a <> false then b else raise Type_error
            | _ -> raise Type_error

and check_type env t a =
    if infer_type env t <> a then false else true

(**
    Trying things out.
 *)
let atvar:tvar = "A";;
let btvar:tvar = "B";;
let ctvar:tvar = "C";;


let at:ty = Term atvar;;
let bt:ty = Term btvar;;
let ct:ty = Term ctvar;;

let abt:ty = Impl(at, bt);;
let act:ty = Impl(at, ct);;
let bct:ty = Impl(bt, ct);;
let abact:ty = Impl(abt, act);;


let x:var = "x";;
let f:var = "f";;
let t:var = "t";;
let gvar:var = "g";;


let xvar:tm = Var(x);;
let tvar:tm = Var(t);;
let fvar:tm = Var(f);;
true
let fx:tm = App(fvar, xvar);;
let xf:tm = Abs(x, at, fx);;


let avar:var = "a"
let bvar:var = "b"

let atm:tm = Var(avar)
let btm:tm = Var(bvar)
let gtm:tm = Var(gvar)

let cab:context = [(bvar, abt); (avar, at)]
let ba:tm = App(btm, atm)

let cxt:context = [(x, at); (t, bt)]
let xt:tm = Abs(x, at, tvar)

let example_context = [(f, abt); (gvar, bct); (x, at)]

let gfx:tm = App(gtm, fx)
let xg:tm = Abs(x, at, gfx)
let gxg:tm = Abs(gvar, bct, xg)
let fgxgfx:tm = Abs(f, abt, gxg)

let fa_context:context = [(f, at)]
let fa:tm = Abs(f, at, xvar)

let fa_b_context:context = [(f, at); (x, bt)]
let xfx:tm = Abs(x, bt, fx)
let fxfx:tm = Abs(f, at, xfx)

let fab_context:context = [(f, abt); (x, bt)]
let fabxfx:tm = Abs(f, abt, xfx)

let lxax:tm = Abs(x, at, xvar)
let aat:ty = Impl(at, at)


let () =
    print_string ("Print the example type: ");;
    print_endline (string_of_ty(abact));;

    print_string ("Print application b.a: ");;
    print_string (btvar ^ ":" ^string_of_ty(abt) ^ ".");;
    print_string (atvar ^ ":" ^ string_of_ty(at) ^ ":   ");;
    print_string (string_of_tm(ba) ^ ": ");;
    print_endline (string_of_ty(infer_type (cab) (ba)));;

    print_string ("Print abstraction Lxt: ");;
    print_string (string_of_tm (xt) ^ ":" ^ btvar ^ ":   ");;
    print_endline (string_of_ty(infer_type (cxt) (xt)));;

    (** 1.4 Testing *)
    print_endline (string_of_tm (fgxgfx));;
    print_endline (string_of_ty(infer_type (example_context) (fgxgfx)));;

    (**

    Because OCaml seems to stop after throwing an exception,
    I cannot run all these in succession, which is why I commented them
    out. I tried them, and they all throw Type_error.
    ******************************************************************

    print_endline (string_of_tm fa);;
    print_endline (string_of_ty (infer_type (fa_context) (fa)));;

    print_endline (string_of_tm fxfx);;
    print_endline (string_of_ty (infer_type (fa_b_context) (fxfx)));;

    print_endline (string_of_tm fabxfx);;
    print_endline (string_of_ty (infer_type (fab_context) (fabxfx)));;
    *)
    if check_type (cxt) (lxax) (aat) <> false then print_endline("True") else print_endline("False");;
