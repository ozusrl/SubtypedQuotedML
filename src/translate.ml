open Common

(* translate multi-staged language to language with records *)
let rec translate exp =
  let envvar n = "r" ^ (string_of_int n) in
  function
  | [] -> failwith "translate: empty env list"
  | env :: envs as envlist -> (match exp with
    | IdE i -> IdE i
    | ConstE c -> ConstE c
    | EmpLstE -> EmpLstE
    | AppE (e1, e2) -> AppE (translate e1 envlist, translate e2 envlist)
    | AbsE (Abs (id, body)) ->
        AbsE (Abs (id, translate body (RecUpdE (env, id, (IdE id)) :: envs)))

    | LetInE (Valbind (id, exp), body) ->
        LetInE ( Valbind (id, translate exp envlist)
               , translate body (RecUpdE (env, id, (IdE id)) :: envlist))

    | FixE (id, Abs (arg, body)) -> 
        let env'  = RecUpdE (env, id, (IdE id)) in
        let env'' = RecUpdE (env', arg, (IdE arg)) in
        FixE (id, Abs (arg, translate body (env'' :: envs)))

    | CondE conds ->
        CondE (List.map
          (fun (g, b) -> (translate g envlist, translate b envlist)) conds)

    | BoxE exp ->
        let var = envvar (List.length envlist) in
        AbsE (Abs (var, translate exp (IdE var :: envlist)))

    | UnboxE exp -> AppE (translate exp envs, env)

    | RunE exp -> AppE (translate exp envlist, RecE [])

    | RecUpdE (r, id, v) -> RecUpdE(translate r envlist, id, translate v envlist)

    | SelectE (r, id) -> SelectE (translate r envlist, id)
    
    | RecE fields ->
        RecE (List.map (fun (id, v) -> (id, translate v envlist)) fields)

    | LiftE exp ->
        AbsE (Abs (envvar (List.length envlist), translate exp envlist)))
