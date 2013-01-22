open Common
open Types

exception StageException
exception NotImplemented
exception Failure of string








    in


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
