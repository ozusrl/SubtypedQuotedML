open Sexplib.Sexp
open Sexplib.Conv

exception NotImplemented

type id = string with sexp

type env 'a = (id * 'a ref) list

let env_of_sexp _ _ = []
let sexp_of_env _ _ = sexp_of_string "env"

type dec = Valbind of (id * exp)

and const = CInt of int | CBool of bool

and abs = Abs of id * exp

and binop = Plus | Minus | Mult | Div | Equals

and exp  =
  | IdE      of id
  | ConstE   of const
  | EmpLstE  (* [] expression *)
  | AppE     of exp * exp
  | AbsE     of abs
  | LetInE   of dec * exp
  | FixE     of id * abs
  | CondE    of (exp * exp) list

  (* ValueE is returned from eval_staged when a lifted expression is unboxed *)
  | ValueE   of value
  | BoxE     of exp
  | UnboxE   of exp
  | RunE     of exp
  | LiftE    of exp

  | RecE     of field list
  | SelectE  of (exp * id)
  | RecUpdE  of (exp * id * exp)

and stdfun =
  | StdCurry    of id * (value -> stdfun)
  | StdFunction of id * (value -> value)

and fun_val =
  | StdFun  of stdfun
  | Closure of value env * id * exp

and field = (id * exp)

and value =
  | ConstV  of const
  | ClosV   of fun_val
  | BoxV    of exp
  | ListV   of value list
  | RecV    of (id * value) list
  | UnitV
  with sexp

let show_exp exp    = to_string (sexp_of_exp exp)
let show_val value  = to_string (sexp_of_value value)

let val_type = function
| ConstV (CInt _) -> "TyInt"
| ConstV (CBool _) -> "TyBool"
| ListV _ -> "TyList"
| ClosV _ -> "TyFun"
| BoxV _ -> "TyBox"
| UnitV -> "TyUnit"
| RecV _ -> "TyRec"


exception Not_bound of id
exception TypeMismatch of (string * string)

let bind_value env id value = (id, ref value) :: env

let rec lookup_ref env id = match env with
| []             -> raise (Not_bound id)
| ((x, v) :: xs) -> if x = id then v else lookup_ref xs id

let set_value env id value =
  let ref = lookup_ref env id in
  ref := value

let lookup env id = !(lookup_ref env id)


(* Mappings of OCaml function and standard environment {{{ **********)

let stdenv =
  (* return a object language function from OCaml function with the type of
   * int -> int *)
  let mk_arith_fun id fn =
    (* make OCaml function a curried function in object language *)
    let inner_fun i1 = StdFunction (id, function
      | ConstV (CInt i2) -> ConstV (CInt (fn i1 i2))
      | not_int -> raise (TypeMismatch ("TyInt", val_type not_int)))
    in
    let curry_fun = StdCurry (id, function
      | ConstV (CInt i1) -> inner_fun i1
      | not_int -> raise (TypeMismatch ("TyInt", val_type not_int)))
    in

    ClosV (StdFun curry_fun)
  in
  (* map OCaml's `=` function to object language *)
  let eq = ClosV (StdFun (StdCurry ("=", fun v1 ->
    StdFunction ("=", fun v2 -> ConstV (CBool (v1 = v2))))))
  in
  (* list operations *)
  let cons = ClosV (StdFun (StdCurry ("::", fun v1 ->
    StdFunction ("::", function
      | ListV lst -> ListV (v1 :: lst)
      | not_list -> raise (TypeMismatch ("TyList", val_type not_list))))))
  in

  let head_v = ClosV (StdFun (StdFunction ("head", function
    | ListV (h :: t) -> h
    | ListV [] -> raise (Failure "head of empty list")
    | not_list -> raise (TypeMismatch ("TyList", val_type not_list)))))
  in

  let tail_v = ClosV (StdFun (StdFunction ("tail", function
    | ListV (h :: t) -> ListV t
    | ListV [] -> raise (Failure "tail of empty list")
    | not_list -> raise (TypeMismatch ("TyList", val_type not_list)))))
  in

  let empty_ty = ClosV (StdFun (StdFunction ("empty", function
    | ListV [] -> ConstV (CBool true)
    | ListV _  -> ConstV (CBool false)
    | not_list -> raise (TypeMismatch ("TyList", val_type not_list)))))
  in

  let nth = ClosV (StdFun (StdCurry ("nth", function
    | ConstV (CInt n) -> StdFunction ("nth", function
      | ListV lst -> List.nth lst (n-1)
      | not_list  -> raise (TypeMismatch ("TyList", val_type not_list)))
    | not_int -> raise (TypeMismatch ("TyInt", val_type not_int)))))
  in

  (* standard environment *)
  [ ("+",     ref (mk_arith_fun "+" (+)))
  ; ("-",     ref (mk_arith_fun "-" (-)))
  ; ("*",     ref (mk_arith_fun "*" ( * )))
  ; ("/",     ref (mk_arith_fun "/" (/)))
  ; ("=",     ref eq)
  ; ("::",    ref cons)
  ; ("head",  ref head_v)
  ; ("tail",  ref tail_v)
  ; ("empty", ref empty_ty)
  ; ("nth",   ref nth)
  ]

let stdrec =
  let mkIdE id = (id, IdE id) in
  RecE (List.map mkIdE ["+"; "-"; "*"; "="; "::"; "head"; "tail"; "empty"; "nth"])

(* Mappings of OCaml function and standard environment }}} **********)
