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
  | CtxL of (id * exp) list

type ctxs = ctx list

let rec merge_ctxs ctxs1 ctxs2 =
  let rec merge
    (ctxs1 : ctx list) (ctxs2 : ctx list) : ctx list =
    match (ctxs1, ctxs2) with
    | [], [] -> []
    | Ctx (id, exp) :: cs1, Ctx (id', exp') :: cs2 ->
        CtxL [(id, exp); (id', exp')] :: merge cs1 cs2
    | Ctx (id, exp) :: cs1, CtxL l :: cs2 ->
        CtxL ((id, exp) :: l) :: (merge cs1 cs2)
    | CtxL l :: cs1, Ctx (id, exp) :: cs2 ->
        CtxL (l @ [(id, exp)]) :: merge cs1 cs2
    | CtxL l1 :: cs1, CtxL l2 :: cs2 ->
        CtxL (l1 @ l2) :: merge cs1 cs2
    | _ -> failwith "fail in merge (ctxs1 ctxs2 ile ayni uzunlukta degil mi?)"
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
| CtxL [(hole, exp)] -> app_of_ctx (Ctx (hole, exp)) body
| CtxL l ->
    let rec iter ctxl accum = match ctxl with
    | [] -> accum
    | (hole, exp) :: cx -> iter cx (AppE (AbsE (Abs (hole, body)), exp))
    in
    let l' = List.rev l in
    iter (List.tl l') (app_of_ctx (Ctx (List.hd l')) body)

(* translate multi-staged language to language with records *)
let rec translate exp : exp list -> (exp * ctx list) =
  let envvar n = "r" ^ (string_of_int n) in
  let holevar n = "h" ^ (string_of_int n) in
  function
  | [] -> failwith "translate: empty env list"
  | env :: rest_envs as envlist -> (match exp with

    | IdE i -> (SelectE (env, i), [])

    | ConstE c -> (ConstE c, [])

    | EmpLstE -> (EmpLstE, [])

    | AppE (e1, e2) ->
        let (e1', k1) = translate e1 envlist in
        let (e2', k2) = translate e2 envlist in
        (AppE (e1', e2'), merge_ctxs k1 k2)

    | AbsE (Abs (id, body)) ->
        let env' = RecUpdE (env, id, IdE id) in
        let body', ctxs = translate body (env' :: envlist) in
        (AbsE (Abs (id, body')), ctxs)

    | LetInE (Valbind (id, value), body) ->
        let value', ctx1 = translate value envlist in
        let body', ctx2 =
          translate value (RecUpdE (env, id, IdE id) :: rest_envs) in
        (LetInE (Valbind (id, value'), body'), merge_ctxs ctx1 ctx2)

    | FixE (id, (Abs (arg, body))) ->
        let env' = RecUpdE (env, id, IdE id) in
        let env'' = RecUpdE (env', arg, IdE arg) in
        let body', ctxs = translate body (env'' :: envlist) in
        (FixE (id, (Abs (arg, body'))), ctxs)

    | CondE e -> raise NotImplemented
    | ValueE _ -> failwith "ValueE in translate"

    | BoxE exp ->
        let var = env_var () in
        let env' = IdE var :: envlist in
        let (exp', k) = translate exp env' in
        (match k with
         | [] -> (AbsE (Abs (var, exp')), [])
         | k :: kx -> ((app_of_ctx k (AbsE (Abs (var, exp')))), kx))

    | UnboxE exp ->
        let exp', ctxs = translate exp rest_envs in
        let var = env_var () in
        (AppE (IdE var, env), Ctx (var, exp') :: ctxs)

    | RunE exp ->
        let var = env_var () in
        let exp', ctxs = translate exp envlist in
        (LetInE (Valbind (var, exp'), AppE (IdE var, RecE [])), ctxs)


    | LiftE exp -> raise NotImplemented
    | RecE _ | SelectE _ | RecUpdE _ -> failwith "record expressions in translate")
