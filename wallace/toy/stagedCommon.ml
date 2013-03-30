type id = string

type 'a env = (id * 'a ref) list

type toplevel =
  | Exp of exp
  | Decl of dec

and dec = Valbind of (id * exp)

and const = CInt of int | CBool of bool

and abs = Abs of id * exp

and exp  =
  | IdE      of id
  | ConstE   of const
  | EmpLstE  (* [] expression *)
  | AppE     of exp * exp
  | AbsE     of abs
  | LetInE   of dec * exp
  | FixE     of id * abs
  | IfE      of (exp * exp * exp)

  | RefE     of exp
  | DerefE   of exp
  | AssignE  of (exp * exp)

  | PairE    of (exp * exp)

  (* ValueE is returned from eval_staged when a lifted expression is unboxed *)
  | ValueE   of value
  | BoxE     of exp
  | UnboxE   of exp
  | RunE     of exp
  | LiftE    of exp

  | EmptyRecE
  | SelectE  of (exp * id)
  | RecUpdE  of (exp * id * exp)

  | SeqE of (exp * exp)

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
  | PairV   of (value * value)
  | BoxV    of exp
  | ListV   of value list
  | RecV    of (id * value) list
  | RefV    of value ref
  | UnitV

exception NotImplemented of exp

let rec val_type = function
| ConstV (CInt _) -> "TyInt"
| ConstV (CBool _) -> "TyBool"
| PairV _ -> "TyPair"
| ListV _ -> "TyList"
| ClosV _ -> "TyFun"
| BoxV _ -> "TyBox"
| UnitV -> "TyUnit"
| RecV _ -> "TyRec"
| RefV v -> "TyRef " ^ val_type !v


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

(* expansive test --{{{-------------------------------------------------------*)

(* stagedv: returns whether expression is valid in staege n *)
let rec stagedv n exp =
  if n = 0 then
    match exp with
    | ConstE _ | AbsE _ | FixE _ -> true
    | BoxE e -> stagedv 1 e
    | _ -> false
  else
    match exp with
    | IdE _ | ConstE _ | EmpLstE -> true
    | AppE (e1, e2) -> stagedv n e1 && stagedv n e2
    | AbsE (Abs(_, e)) -> stagedv n e
    | LetInE (Valbind (_, e), body) -> stagedv n e && stagedv n body
    | FixE (_, Abs (_, e)) -> stagedv n e
    | IfE (e1, e2, e3) -> stagedv n e1 && stagedv n e2 && stagedv n e3
    | RefE e | DerefE e -> stagedv n e
    | AssignE (e1, e2)
    | PairE (e1, e2) -> stagedv n e1 && stagedv n e2

    | ValueE _ -> failwith "ValueE in stagedv test"
    | BoxE e   -> stagedv (n+1) e
    | UnboxE e -> if n = 1 then false else stagedv (n-1) e
    | RunE e   -> stagedv n e
    | LiftE e  -> stagedv n e

    | EmptyRecE -> true
    | SelectE (e, _) -> stagedv n e
    | RecUpdE (e1, _, e2) -> stagedv n e1 && stagedv n e2
    | SeqE (e1, e2) -> stagedv n e1 && stagedv n e2

(* expansive: returns whether expression can expand to a store *)
let rec expansive n = function
| IdE _    -> false
| ConstE _ -> false
| EmpLstE  -> false
| AppE _   -> true
| AbsE (Abs (_, exp)) | FixE (_, Abs (_, exp)) ->
    if n = 0 || stagedv 1 exp then false else true
| LetInE (Valbind (_, e1), e2) -> expansive n e1 || expansive n e2
| IfE (e1, e2, e3) -> expansive n e1 || expansive n e2 || expansive n e3

| RefE _   -> true
| DerefE e -> expansive n e
| AssignE (e1, e2) -> expansive n e1 || expansive n e2

| PairE (e1, e2) -> expansive n e1 || expansive n e2

| ValueE e  -> failwith "ValueE in expansive."
| BoxE e    -> if stagedv 1 e then false else true
| UnboxE e  -> true
| RunE e    -> true (* TODO *)
| LiftE e   -> expansive n e
| EmptyRecE -> false
| SelectE (e, _)      -> expansive n e
| RecUpdE (e1, _, e2) -> expansive n e1 || expansive n e2
| SeqE (e1, e2)       -> expansive n e1 || expansive n e2

(* ---}}}---------------------------------------------------------------------*)

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
      | ListV lst -> List.nth lst n
      | not_list  -> raise (TypeMismatch ("TyList", val_type not_list)))
    | not_int -> raise (TypeMismatch ("TyInt", val_type not_int)))))
  in

  let fst = ClosV (StdFun (StdFunction ("fst", function
    | PairV (v1, v2) -> v1
    | not_pair       -> raise (TypeMismatch ("TyPair", val_type not_pair)))))
  in

  let snd = ClosV (StdFun (StdFunction ("snd", function
    | PairV (v1, v2) -> v2
    | not_pair       -> raise (TypeMismatch ("TyPair", val_type not_pair)))))
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
  ; ("fst",   ref fst)
  ; ("snd",   ref snd)
  ]

(* Mappings of OCaml function and standard environment }}} **********)
