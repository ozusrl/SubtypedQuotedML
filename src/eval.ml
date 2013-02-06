open Common

exception StageException
exception Failure of string
exception UnsupportedExp of exp
exception SelectException of (exp * id)

module type Eval = sig
  val eval : value env -> exp -> value
  val run : exp -> value
end

module EvalBase (EvalExtend : Eval) : Eval = struct

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

  | FixE (id, Abs (arg, body)) ->
      let env' = bind_value env id UnitV in
      let clos = ClosV (Closure (env', arg, body)) in
      set_value env' id clos; clos

  | CondE [] -> UnitV
  | CondE ((g, e) :: r) -> (match eval env g with
    | ConstV (CBool true) -> eval env e
    | _ -> eval env (CondE r))

  | e -> EvalExtend.eval env e

  (* Eval }}} *********************************************************)

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

  let run exp = eval stdenv exp
  
end

(* evaluator for the staged calculus *)
module rec StagedEval : Eval = struct

  module CoreEval = EvalBase(StagedEval)

  let rec eval env = function
  | BoxE exp    -> BoxV (eval_staged env exp 1)
  | RunE exp -> (match CoreEval.eval env exp with
    | BoxV code -> CoreEval.eval stdenv code
    | not_code -> raise (TypeMismatch ("CodeTy", val_type not_code)))

  | UnboxE _ -> failwith "can't unbox in stage 0"
  | LiftE exp -> BoxV (ValueE (CoreEval.eval env exp))
  | ValueE v -> v

  | exp -> failwith ("Unrecognized expression " ^ (show_exp exp) ^ " in StagedEval.eval.")

  (* Staged computations {{{ *****************************************)
  and eval_staged env exp n = match exp with
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
      let cond_rest = (match eval_staged env (CondE r) n with
      | CondE e -> e
      | _ -> failwith "") in (* suppress warning *)
      CondE ((g', b') :: cond_rest)

  | ValueE v -> ValueE v
  | BoxE exp -> BoxE (eval_staged env exp (n+1))
  | UnboxE exp ->
      if n < 1 then raise StageException
      else begin if n = 1 then
        match CoreEval.eval env exp with
        | BoxV exp' -> exp'
        | _ -> failwith "unboxing a non-code value"
      else
        UnboxE (eval_staged env exp (n-1))
      end

  | RunE exp -> RunE (eval_staged env exp n)
  | LiftE exp -> LiftE (eval_staged env exp n)

  | exp -> failwith ("Unrecognized expression " ^ (show_exp exp) ^ " in StagedEval.eval_staged.")
  (* Staged computations }}} ******************************************)

  let run = CoreEval.run
  
end


(* evaluator for the record calculus *)
module rec RecordEval : Eval = struct

  module CoreEval = EvalBase(RecordEval)

  let rec eval env = function
  | RecE fields ->
      let field_vals = List.map (fun (id, e) -> (id, CoreEval.eval env e)) fields in
      RecV field_vals
  | SelectE (record, field) -> (match CoreEval.eval env record with
    | RecV fields ->
        (try
          snd (List.find (fun (i, v) -> i = field) fields)
        with Not_found -> failwith ("Not_found: " ^ field))
    | not_rec -> raise (TypeMismatch ("RecTy", val_type not_rec)))
  | RecUpdE (record, id, value) -> (match CoreEval.eval env record with
    | RecV fields -> RecV ((id, CoreEval.eval env value) :: fields)
    | not_rec -> raise (TypeMismatch ("RecTy", val_type not_rec)))

  | exp -> failwith ("Unrecognized expression " ^ (show_exp exp) ^ " in RecordEval.eval.")

  let run = CoreEval.run

end
