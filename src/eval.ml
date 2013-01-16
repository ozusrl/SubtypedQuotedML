open Common

exception Not_bound of id
exception TypeMismatch of (string * string)
exception StageException
exception NotImplemented
exception Failure of string

(* Environment operations {{{ ***************************************)

let bind_value env id value = (id, ref value) :: env

let rec lookup_ref env id = match env with
| []             -> raise (Not_bound id)
| ((x, v) :: xs) -> if x = id then v else lookup_ref xs id

let set_value env id value =
  let ref = lookup_ref env id in
  ref := value

let lookup env id = !(lookup_ref env id)

(* Environment operations }}} ***************************************)

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

  (* standard environment *)
  [ ("+", ref (mk_arith_fun "+" (+)))
  ; ("-", ref (mk_arith_fun "-" (-)))
  ; ("*", ref (mk_arith_fun "*" ( * )))
  ; ("/", ref (mk_arith_fun "/" (/)))
  ; ("=", ref eq)
  ; ("::", ref cons)
  ; ("head", ref head_v)
  ; ("tail", ref tail_v)
  ; ("empty", ref empty_ty)
  ]

(* Mappings of OCaml function and standard environment }}} **********)

(* Eval {{{ *********************************************************)

let rec eval env = function
| IdE id -> lookup env id
| ConstE c -> ConstV c
| EmpLstE -> ListV []

| AppE (f, p) ->
    let f' = eval env f in
    let p' = eval env p in
    apply f' p'

| AbsE (Abs (arg_id, body)) ->
    ClosV (Closure (env, arg_id, body))

| LetInE (Valbind (arg, exp), body) ->
    let value = eval env exp in
    let env' = bind_value env arg value in
    eval env' body

| FixE (id, (Abs (arg, body))) ->
    let env' = bind_value env id UnitV in
    let clos = ClosV (Closure (env', arg, body)) in
    set_value env' id clos; clos

| CondE [] -> UnitV
| CondE ((g, e) :: r) -> (match eval env g with
  | ConstV (CBool true) -> eval env e
  | _ -> eval env (CondE r))

| ValueE value -> value
| BoxE exp    -> BoxV (eval_staged env exp 1)
| UnboxE (BoxE exp) -> eval env (eval_staged env exp 1)
| UnboxE not_box -> raise (Failure "unboxing a non-box value")
| RunE exp -> (match eval env exp with
  | BoxV code -> eval env code
  | not_code -> raise (TypeMismatch ("CodeTy", val_type not_code)))
| LiftE exp -> BoxV (ValueE (eval env exp))

(* Eval }}} *********************************************************)

(* Staged computations {{{ *****************************************)

and eval_staged env exp n = (match exp with
| IdE id -> IdE id
| ConstE e -> ConstE e
| EmpLstE -> EmpLstE
| AppE (f, p) -> AppE (eval_staged env f n, eval_staged env p n)
| AbsE (Abs (id, body)) -> AbsE (Abs (id, eval_staged env body n))
| LetInE (Valbind (id, exp), body) ->
    LetInE (Valbind (id, eval_staged env exp n), eval_staged env body n)
| FixE (id, (Abs (arg, body))) ->
    FixE (id, (Abs (arg, eval_staged env body n)))

| CondE [] -> CondE []
| CondE ((g, b) :: r) ->
    let g' = eval_staged env g n in
    let b' = eval_staged env b n in
    let (CondE cond_rest) = eval_staged env (CondE r) n in
    CondE ((g', b') :: cond_rest)

| ValueE exp -> ValueE exp
| BoxE exp -> BoxE (eval_staged env exp (n+1))
| UnboxE exp ->
    if n < 1 then raise StageException
    else begin if n = 1 then
      match eval env exp with
      | BoxV exp' -> exp'
      | ConstV c -> ConstE c
      | _ -> raise StageException
    else
      UnboxE (eval_staged env exp (n-1))
    end

| RunE exp -> RunE (eval_staged env exp n)
| LiftE exp -> LiftE (eval_staged env exp n))

(* Staged computations }}} *****************************************)

(* Function application {{{ *****************************************)

and apply f arg =
  let apply_stdfun stdfun arg = match stdfun with
  | StdCurry (id, fn) -> ClosV (StdFun (fn arg))
  | StdFunction (id, fn) -> fn arg
  in
  match f with
  | ClosV (StdFun stdfun) -> apply_stdfun stdfun arg
  | ClosV (Closure (env, id, body)) ->
      let env' = bind_value env id arg in
      eval env' body
  | not_clos -> raise (TypeMismatch ("FunTy", val_type not_clos))

(* Function application }}} *****************************************)
