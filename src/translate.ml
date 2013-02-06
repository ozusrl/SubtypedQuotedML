open Common


let last_env_var = ref 0
let env_var _ =
  let r = !last_env_var in
  last_env_var := r + 1; "r" ^ (string_of_int r)

let last_hole_var = ref 0
let hole_var _ =
  let r = !last_hole_var in
  last_hole_var := r + 1; "h" ^ (string_of_int r)

type ctx  =
  | Ctx  of (id * exp) (* hole variable, box value *)
  | CtxL of (id * ctx * exp)

type ctxs = ctx list

let rec merge_ctxs ctxs1 ctxs2 =
  let rec merge_ctx k1 k2 =
    match k1 with
    | Ctx(id, exp) -> CtxL(id, k2, exp)
    | CtxL(id, k3, exp) -> CtxL(id, merge_ctx k3 k2, exp)
  in
  let rec merge
    (ctxs1 : ctx list) (ctxs2 : ctx list) : ctx list =
    match (ctxs1, ctxs2) with
    | [], [] -> []
    | ctx1 :: cs1, ctx2 :: cs2 -> merge_ctx ctx1 ctx2 :: merge cs1 cs2
    | _ -> failwith "fail in merge (ctxs1 and ctxs2 are not )"
  in

  let rec iter ctxs1 ctxs2 c =
    if c == 0 then
      merge ctxs1 ctxs2
    else begin if c < 0 then
      List.hd ctxs2 :: iter ctxs1 (List.tl ctxs2) (c+1)
    else
      List.hd ctxs1 :: iter (List.tl ctxs1) ctxs2 (c-1)
    end
  in

  let ctx1_len = List.length ctxs1 in
  let ctx2_len = List.length ctxs2 in
  iter ctxs1 ctxs2 (ctx1_len - ctx2_len)


let rec app_of_ctx ctx body = match ctx with
| Ctx (hole, exp) -> AppE (AbsE (Abs (hole, body)), exp)
| CtxL (hole, k, exp) -> AppE (AbsE (Abs (hole, app_of_ctx k body)), exp)


(* translate multi-staged language to language with records *)
let rec translate exp envList : (exp * ctxs) =
  match envList with
  | [] -> failwith "translate: empty env list"
  | env :: rest_envs -> (match exp with

    | IdE i -> (SelectE (env, i), []) (* Hmm... *)

    | ConstE c -> (ConstE c, [])

    | EmpLstE -> (EmpLstE, [])

    | AppE (e1, e2) ->
        let (e1', k1) = translate e1 envList in
        let (e2', k2) = translate e2 envList in
        (AppE (e1', e2'), merge_ctxs k1 k2)

    | AbsE (Abs (id, body)) ->
        let env' = RecUpdE (env, id, IdE id) in
        let body', ctxs = translate body (env' :: rest_envs) in
        (AbsE (Abs (id, body')), ctxs)

    | LetInE (Valbind (id, value), body) ->
        let value', ctx1 = translate value envList in
        let body', ctx2 =
          translate body (RecUpdE (env, id, IdE id) :: rest_envs) in
        (LetInE (Valbind (id, value'), body'), merge_ctxs ctx1 ctx2)

    | FixE (id, (Abs (arg, body))) ->
        let env' = RecUpdE (env, id, IdE id) in
        let env'' = RecUpdE (env', arg, IdE arg) in
        let body', ctxs = translate body (env'' :: envList) in
        (FixE (id, (Abs (arg, body'))), ctxs)

    | CondE [] -> (CondE [], [])
    | CondE ((g, b) :: r) ->
        let g', ctx = translate g envList in
        let b', ctx' = translate b envList in
        let ((CondE rest), ctx'') = translate (CondE r) envList in
        (CondE ((g', b') :: rest), merge_ctxs ctx (merge_ctxs ctx' ctx''))

    | BoxE exp ->
        let var = env_var () in
        let env' = IdE var :: envList in
        let (exp', k) = translate exp env' in
        (match k with
         | [] -> (AbsE (Abs (var, exp')), [])
         | k :: kx -> (app_of_ctx k (AbsE (Abs (var, exp'))), kx))

    | UnboxE exp ->
        let exp', ctxs = translate exp rest_envs in
        let var = hole_var () in
        (AppE (IdE var, env), Ctx (var, exp') :: ctxs)

    | RunE exp ->
        let var = env_var () in
        let exp', ctxs = translate exp envList in
        (LetInE (Valbind (var, exp'), AppE (IdE var, stdrec)), ctxs)

    | LiftE exp -> 
        let rvar, hvar = env_var(), hole_var() in
        let exp', ctx = translate exp envList in
        (LetInE (Valbind (hvar, exp'), AbsE(Abs(rvar, IdE hvar))), ctx)
    | ValueE v -> failwith "ValueE is not expected to occur in translation."
    | RecE _ | SelectE _ | RecUpdE _ -> failwith "record expressions in translate")
