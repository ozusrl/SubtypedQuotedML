open Sexplib.Sexp
open Sexplib.Conv

type id = string with sexp

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

and env 'a = (id * 'a ref) list

and stdfun =
  | StdCurry    of id * (value -> stdfun)
  | StdFunction of id * (value -> value)

and fun_val =
  | StdFun  of stdfun
  | Closure of value env * id * exp

and value =
  | ConstV of const
  | ClosV  of fun_val
  | BoxV   of exp
  | ListV  of value list
  | UnitV
  with sexp

let show_exp exp = to_string (sexp_of_exp exp)

and show_const = function
| CInt i -> string_of_int i
| CBool b -> string_of_bool b

and show_stdfun = function
| StdCurry (id, _)
| StdFunction (id, _) -> "<function: " ^ id ^ ">"

and show_val value = to_string (sexp_of_value value)

let val_type = function
| ConstV (CInt _) -> "TyInt"
| ConstV (CBool _) -> "TyBool"
| ListV _ -> "TyList"
| ClosV _ -> "TyFun"
| BoxV _ -> "TyBox"
| UnitV -> "TyUnit"
