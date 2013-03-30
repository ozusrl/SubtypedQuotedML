open StagedCommon


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


type translationEnv = Empty | Rho of string | Cons of translationEnv * string

let rec recordExp_of_env env =
  match env with
  | Empty -> EmptyRecE
  | Rho rho -> IdE rho
  | Cons(env',x) -> RecUpdE(recordExp_of_env env', x, IdE x)

(* translate multi-staged language to language with records *)
let translate translateEnv exp envStack : (exp * ctxs) =
  let rec lookup env x =
    match env with
    | Empty -> if List.mem x translateEnv then IdE x
               else failwith ("Id " ^ x ^ " not found in the translation env.")
    | Rho rho -> if List.mem x translateEnv then IdE x
                 else SelectE(IdE rho, x)
    | Cons(r', y) -> if x = y then IdE x else lookup r' x
  in
  let rec translate exp envStack = match envStack with
  | [] -> failwith "translate: empty env list"
  | env :: rest_envs -> (match exp with

    | ConstE c -> (ConstE c, [])

    | IdE i -> ((lookup env i), [])

    | PairE (e1, e2) ->
        let (e1', ctxs1) = translate e1 envStack in
        let (e2', ctxs2) = translate e2 envStack in
        (PairE (e1', e2'), merge_ctxs ctxs1 ctxs2)

    | EmpLstE -> (EmpLstE, [])

    | AppE (e1, e2) ->
        let (e1', ctxs1) = translate e1 envStack in
        let (e2', ctxs2) = translate e2 envStack in
        (AppE (e1', e2'), merge_ctxs ctxs1 ctxs2)

    | AbsE (Abs (id, body)) ->
        let env' = Cons(env, id) :: rest_envs in
        let body', ctxs = translate body env' in
        (AbsE (Abs (id, body')), ctxs)

    | LetInE (Valbind (id, value), body) ->
        let value', ctxs1 = translate value envStack in
        let body', ctxs2 =
          translate body (Cons(env, id) :: rest_envs) in
        (LetInE (Valbind (id, value'), body'), merge_ctxs ctxs1 ctxs2)

    | FixE (id, (Abs (arg, body))) ->
        let env' = Cons(env, id) in
        let env'' = Cons(env', arg) in
        let body', ctxs = translate body (env'' :: rest_envs) in
        (FixE (id, (Abs (arg, body'))), ctxs)

    | IfE (e1, e2, e3) ->
        let e1', ctxs = translate e1 envStack in
        let e2', ctxs' = translate e2 envStack in
        let e3', ctxs'' = translate e3 envStack in
        (IfE (e1', e2', e3'), merge_ctxs ctxs (merge_ctxs ctxs' ctxs''))

    | RefE exp ->
        let exp', ctxs = translate exp envStack in
        (RefE exp', ctxs)
    | DerefE exp ->
        let exp', ctxs = translate exp envStack in
        (DerefE exp', ctxs)
    | AssignE (e1, e2) ->
        let e1', ctxs1 = translate e1 envStack in
        let e2', ctxs2 = translate e2 envStack in
        (AssignE (e1', e2'), merge_ctxs ctxs1 ctxs2)

    | BoxE exp ->
        let rho = env_var () in
        let newEnv = Rho rho in
        let env' = newEnv :: envStack in
        let (exp', k) = translate exp env' in
        (match k with
         | [] -> (AbsE (Abs (rho, exp')), [])
         | k :: kx -> (app_of_ctx k (AbsE (Abs (rho, exp'))), kx))

    | UnboxE exp ->
        let exp', ctxs = translate exp rest_envs in
        let h = hole_var () in
        let r = recordExp_of_env env in
        (AppE (IdE h, r), Ctx(h, exp') :: ctxs)

    | RunE exp ->
        let h = hole_var () in
        let exp', ctxs = translate exp envStack in
        (LetInE (Valbind (h, exp'), AppE (IdE h, EmptyRecE)), ctxs)

    | LiftE exp ->
        let rho, h = env_var(), hole_var() in
        let exp', ctxs = translate exp envStack in
        (LetInE (Valbind (h, exp'), AbsE(Abs(rho, IdE h))), ctxs)
    | ValueE v -> failwith "ValueE is not expected to occur in translation."

    | SeqE (e1, e2) ->
        let e1', ctxs  = translate e1 envStack in
        let e2', ctxs' = translate e2 envStack in
        (SeqE (e1', e2'), merge_ctxs ctxs ctxs')

    | EmptyRecE | SelectE _ | RecUpdE _ -> failwith "record expressions in translate")
  in
  translate exp envStack


let translate env exp = translate env exp [Empty]
