
type id = string

type dec = Valbind of (id * exp)

and const_exp = CInt of int | CBool of bool

and abs = Abs of id * exp

and binexp =
    Plus     of exp * exp
  | Minus    of exp * exp
  | Mult     of exp * exp
  | Div      of exp * exp
  | Equals   of exp * exp

and exp  =
    IdE      of id
  | ConstE   of const_exp
  | AppE     of exp * exp
  | AbsE     of abs
  | BoxE     of exp
  | UnboxE   of exp
  | RunE     of exp
  | LetInE   of dec * exp
  | LiftE    of exp
  | FixE     of id * abs
  | CondE    of (exp * exp) list
  | BinaryE  of binexp


let show_exp exp = "ok"
let show_val v = "ok"
