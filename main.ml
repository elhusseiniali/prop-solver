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


let rec infer_type c t =
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
                                if x <> infer_type c a
                                then raise Type_error
                                else y
                            )
                    )
                    | _ -> raise Type_error
                    (** I think we don't reach this because of left-associativity *)
                )
            | Abs(a, tp, b) -> (
                Impl(tp, infer_type (c) (b))
            )
    with
        | Not_found -> raise Type_error


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
let act:ty = Impl(at, ct);;                (*
                match tp with
                | Term(x) -> Impl(tp, infer_type (c) (b))
                | Impl(x, y) -> ( Impl(tp, infer_type (c) (b))

                    match infer_type (c) (b) with
                        | Term(x1) -> if x <> Term x1 then raise Type_error else y
                        | Impl(x1, y1) -> (
                            Impl(tp, infer_type (c) (b))

                            match x1 with
                            | Term(x2) -> Impl(tp, infer_type (c) (b))
                            | Impl(x2, y2) ->
                                if x <> x2 then Impl(tp, infer_type (c) (b)) else
                                Impl(tp, infer_type (c) (b))
                            if x1 <> x then
                            Impl(tp, infer_type (c) (b))
                            else raise Type_error
                            *)
let bct:ty = Impl(bt, ct);;
let abact:ty = Impl(abt, act);;


let x:var = "x";;
let f:var = "f";;
let t:var = "t";;
let gvar:var = "g";;


let xvar:tm = Var(x);;
let tvar:tm = Var(t);;
let fvar:tm = Var(f);;

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
