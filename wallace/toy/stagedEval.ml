open StagedCommon
open StagedPrint

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

  | IfE (guard, thenE, elseE) -> (match eval env guard with
    | ConstV (CBool true) -> eval env thenE
    | _ -> eval env elseE)

  | RefE exp -> RefV (ref (eval env exp))
  | DerefE exp -> (match eval env exp with
    | RefV ref -> !ref
    | not_ref ->
        let val_str = sprint (fun _ -> print_value not_ref) in
        failwith ("dereferencing a non-ref value: " ^ val_str))
  | AssignE (e1, e2) -> (match eval env e1 with
    | RefV ref ->
      let v = eval env e2 in
      ref := v; UnitV
    | not_ref ->
        let val_str = sprint (fun _ -> print_value not_ref) in
        failwith ("assigning to a non-ref value: " ^ val_str))
  | SeqE (e1, e2) -> let _ = eval env e1 in eval env e2

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
  | exp ->
      let exp_str = sprint (fun _ -> print_exp exp) in
      failwith ("Unrecognized expression " ^ exp_str ^ " in StagedEval.eval.")

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

  | IfE (g, t, e) ->
      let g' = eval_staged env g n in
      let t' = eval_staged env t n in
      let e' = eval_staged env e n in
      IfE (g', t', e')

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

  | exp ->
      let exp_str = sprint (fun _ -> print_exp exp) in
      failwith ("Unrecognized expression " ^ exp_str ^ " in StagedEval.eval_staged.")

  (* Staged computations }}} ******************************************)

  let run = CoreEval.run

end


(* evaluator for the record calculus *)
module rec RecordEval : Eval = struct

  module CoreEval = EvalBase(RecordEval)

  let rec eval env = function
  | EmptyRecE -> RecV []
  | SelectE (record, field) -> (match CoreEval.eval env record with
    | RecV fields ->
        (try
          snd (List.find (fun (i, v) -> i = field) fields)
        with Not_found -> failwith ("Not_found: " ^ field))
    | not_rec -> raise (TypeMismatch ("RecTy", val_type not_rec)))
  | RecUpdE (record, id, value) -> (match CoreEval.eval env record with
    | RecV fields -> RecV ((id, CoreEval.eval env value) :: fields)
    | not_rec -> raise (TypeMismatch ("RecTy", val_type not_rec)))

  | exp ->
      let exp_str = sprint (fun _ -> print_exp exp) in
      failwith ("Unrecognized expression " ^ exp_str ^ " in RecordEval.eval.")

  let run = CoreEval.run

end
