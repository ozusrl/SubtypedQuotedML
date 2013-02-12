open Common

(*  misc. --{{{-------------------------------------------------------------- *)

let rec union xs ys = match xs with
| []      -> ys
| x :: xr ->
    if List.mem x ys then
      union xr ys
    else
      x :: union xr ys

let rec unique = function
| [] -> []
| x :: xr -> if List.mem x xr then unique xr else x :: unique xr

module IdSet = Set.Make(struct
    let compare = Pervasives.compare
    type t = string
end)

(* ---}}}-------------------------------------------------------------------- *)

(* language of types --{{{----------------------------------------------------*)

type ty =
  | TInt
  | TBool

  | TList of ty
  | TRef  of ty

  | TFun of ty * ty
  | TRec of tyrec
  | TVar of typevar
and tyrec =
  | EmptyRec
  | Rho of recvar
  | Row of id * field * tyrec
and field =
  | FieldType of ty
  | FieldVar of fieldvar
  | Bot
and 'a link =
  | NoLink of id
  | LinkTo of 'a

and typevar = (tyvarlink * int) ref
and tyvarlink = ty link

and fieldvar = (fieldvarlink * int) ref
and fieldvarlink = field link

and recvar = (recvarlink * int * id list) ref
and recvarlink = tyrec link

type tyscm = TypeScheme of id list * ty

type tenv = (id * tyscm) list

(* ---}}}---------------------------------------------------------------------*)

(* link operations ---{{{-----------------------------------------------------*)

let last_tyvar = ref 0
let new_nolink _ =
  let rec mkname i res =
    if i < 26 then
      Char.chr (97+i) :: res
    else
      mkname (i/26-1) (Char.chr (97 + i mod 26) :: res)
  in
  let int_to_name i =
    let name_lst = mkname i [] in
    let len = List.length name_lst in
    let str = String.make len ' ' in
    for j = 0 to (len-1) do
      str.[j] <- List.nth name_lst j
    done;
    str
  in
  last_tyvar := !last_tyvar + 1;
  NoLink (int_to_name !last_tyvar)

let new_typevar (lvl : int) : typevar =
  ref (new_nolink (), lvl)

let new_recvar (lvl : int) (ids : id list) : recvar =
  ref (new_nolink (), lvl, ids)

let new_fieldvar (lvl : int) : fieldvar =
  ref (new_nolink (), lvl)

let set_link tyvar newlink =
  let (_, lvl) = !tyvar in
  tyvar := (newlink, lvl)

let set_link_level tyvar newlvl =
  let (link, _) = !tyvar in
  tyvar := (link, newlvl)

(* ---}}}---------------------------------------------------------------------*)

(* type scheme instantiation ---{{{-------------------------------------------*)

let instantiate lvl (TypeScheme (tvs, t)) : ty =
  if List.length tvs = 0 then
    t
  else
    let rec subst ty tvs =
      if List.length tvs = 0 then ty
      else
        let rec subst_tyrec tyrec tvs =
          if List.length tvs = 0 then tyrec
          else
            match tyrec with
            | EmptyRec -> EmptyRec
            | Rho recvar -> (match !recvar with
              | (NoLink id, i, ids) ->
                  if id = List.hd tvs then Rho (new_recvar i ids)
                  else subst_tyrec tyrec (List.tl tvs)
              | (LinkTo _, _, _) -> tyrec)
            | Row (id, field, tyrec) -> Row (id, field, subst_tyrec tyrec tvs)
        in
        match ty with
        | TInt  -> TInt
        | TBool -> TBool
        | TList ty -> TList (subst ty tvs)
        | TRef  ty -> TRef (subst ty tvs)
        | TFun (ty1, ty2) -> TFun (subst ty1 tvs, subst ty2 tvs)
        | TRec tyrec -> TRec (subst_tyrec tyrec tvs)
        | TVar typevar -> (match !typevar with
          | NoLink id, i ->
              if id = List.hd tvs then TVar (new_typevar i) else subst ty (List.tl tvs)
          | LinkTo _, _ -> TVar typevar)
    in
    subst t tvs

(* ---}}}---------------------------------------------------------------------*)

(* norm ty ---{{{-------------------------------------------------------------*)

let rec norm_ty =
  let rec norm_tyrec =
    let rec norm_recvar = function
    | (NoLink id, lvl, ids) -> (NoLink id, lvl, ids)
    | (LinkTo link, lvl, ids) ->
        let link' = norm_tyrec link in
        (LinkTo link', lvl, ids)
    in
    function
    | EmptyRec -> EmptyRec
    | Row (id, field, tyrec) -> Row (id, field, norm_tyrec tyrec)
    | Rho recvar ->
        let recvar' = norm_recvar !recvar in
        recvar := recvar';
        Rho recvar
  in
  let rec norm_typevar typevar = match !typevar with
  | NoLink id, lvl -> TVar (ref (NoLink id, lvl))
  | LinkTo link, lvl -> (match norm_ty link with
    | TVar typevar -> norm_typevar typevar
    | t -> t)
  in
  function
  | TInt     -> TInt
  | TBool    -> TBool
  | TList ty -> TList (norm_ty ty)
  | TRef  ty -> TRef  (norm_ty ty)
  | TFun (ty1, ty2) -> TFun (norm_ty ty1, norm_ty ty2)
  | TRec tyrec   -> TRec (norm_tyrec tyrec)
  | TVar typevar -> norm_typevar typevar

(* ---}}}---------------------------------------------------------------------*)

(* norm ty ---{{{-------------------------------------------------------------*)

let rec freetyvars ty =
  let rec freetyvars_tyrec = function
  | EmptyRec -> []
  | Rho recvar -> (match !recvar with
    | (NoLink id, lvl, _) -> [id,lvl]
    | (LinkTo l, _, _) -> freetyvars_tyrec l)
  | Row (_, _, tyrec) -> freetyvars_tyrec tyrec
  in
  let rec freetyvars_typevar = function
  | (NoLink id, lvl) -> [id,lvl]
  | (LinkTo l, lvl) -> freetyvars l
  in
  match norm_ty ty with
  | TInt  -> []
  | TBool -> []
  | TList t
  | TRef  t -> freetyvars t
  | TFun (t1, t2) -> freetyvars t1 @ freetyvars t2
  | TRec tyrec    -> freetyvars_tyrec tyrec
  | TVar typevar  -> freetyvars_typevar !typevar

(* ---}}}---------------------------------------------------------------------*)

(* unification --{{{----------------------------------------------------------*)

let rec link_to_ty (typevar : typevar) (ty : ty) =
  let occur_check (ty : ty) (ids : id list) : bool =
    let freevars = List.map fst (freetyvars ty) in
    let bs = List.map (fun var -> List.mem var ids) freevars in
    List.fold_right (fun a b -> a or b) bs false
  in

  let prune maxlvl tvs =
    let reducelvl typevar = 
      let (_, lvl) = !typevar in
      set_link_level typevar (min lvl maxlvl)
    in
    List.iter reducelvl tvs
  in

  let (_, level) = !typevar in
  let fvs = freetyvars ty in
  if occur_check (TVar typevar) (List.map fst fvs) then
    failwith "type error: circularity"
  else
    prune level fvs; (* TODO *)
    set_link typevar (LinkTo t)

let rec unify (lvl : int) (t1 : ty) (t2 : ty) : unit =
  let t1' = norm_ty t1 in
  let t2' = norm_ty t2 in
  match (t1', t2') with
  | TVar typevar1, TVar typevar2 ->
      if typevar1 = typevar2 then ()
      else
        let (_, tv1level) = !typevar1 in
        let (_, tv2level) = !typevar2 in
        if tv1level < tv2level then
          link_to_ty typevar1 t2'
        else
          link_to_ty typevar2 t1'
  | TVar typevar, ty
  | ty, TVar typevar -> link_to_ty typevar ty

  | TInt,  TInt
  | TBool, TBool -> ()
  | TList ty1, TList ty2 -> unify lvl ty1 ty2
  | TRef ty1,  TRef ty2  -> unify lvl ty1 ty2
  | TFun (a, b), TFun (c, d) -> unify lvl a c; unify lvl b d
  | TRec tyrec1, TRec tyrec2 -> () (* TODO *)

  | t1, t2 -> failwith "can't unify lol"

(* ---}}}---------------------------------------------------------------------*)

(* type inference --{{{-------------------------------------------------------*)

let rec generalize lvl (t : ty) : tyscm =
  let notfreeincontext (_,linklvl) =
    linklvl > lvl
  in
  let tvs = unique (List.map fst (List.filter notfreeincontext (freetyvars t))) in
  TypeScheme (tvs, t)

let rec typ (lvl : int) (env : tenv) : (exp -> ty) = function
| IdE id -> instantiate lvl (List.assoc id env)
| ConstE (CInt _) -> TInt
| ConstE (CBool _) -> TBool
| EmpLstE -> TList (TVar (new_typevar lvl))
| AppE (e1, e2) ->
    let funty = typ lvl env e1 in
    let argty = typ lvl env e2 in
    let retty = TVar (new_typevar lvl) in
    unify lvl funty (TFun (argty, retty));
    retty
| AbsE (Abs (id, body)) ->
    let ptyv       = TVar (new_typevar lvl) in (* parameter type *)
    let f_body_env = (id, TypeScheme([], ptyv)) :: env in
    let rtyp       = typ lvl f_body_env body in (* return value type *)
    TFun (ptyv, rtyp)
| LetInE (Valbind (id, rhs), body) ->
    let rhsty  = typ (lvl+1) env rhs in
    let letenv = (id, generalize lvl rhsty) :: env in
    typ lvl letenv body
| FixE (fname, Abs (id, body)) ->
    let ptyv       = TVar (new_typevar lvl) in
    let rtyv       = TVar (new_typevar lvl) in
    let f_body_env =
      (id, TypeScheme ([], ptyv))
        :: (fname, TypeScheme ([], TFun (ptyv, rtyv)))
        :: env in
    let rtyp = typ lvl f_body_env body in
    unify lvl rtyp rtyv;
    TFun (ptyv, rtyv)
| CondE [] -> failwith "CondE with empty cond list"
| CondE ((guard, body) :: rest) ->
    let rec iter body_ty = function
    | [] -> body_ty
    | (guard, body) :: rest ->
        let guard_ty = typ lvl env guard in
        unify lvl guard_ty TBool;
        let body_ty' = typ lvl env body in
        unify lvl body_ty body_ty';
        iter body_ty rest
    in
    let guard_ty = typ lvl env guard in
    unify lvl guard_ty TBool;
    let body_ty = typ lvl env body in
    iter body_ty rest

(* refs -------------------------------------------*)
| RefE e -> TRef (typ lvl env e)
| DerefE e ->
    let ref_ty = typ lvl env e in
    let ret = TVar (new_typevar lvl) in
    unify lvl ref_ty (TRef ret);
    ret
| AssignE (e1, e2) ->
    let ref_ty = typ lvl env e1 in
    let new_val_ty = typ lvl env e2 in
    unify lvl ref_ty (TRef new_val_ty);
    new_val_ty

(* records --------------------------------------- *)
| RecE rows ->
    let rec typ_of_rows lvl env = function
    | [] -> EmptyRec
    | (id, exp) :: rest ->
        Row (id, FieldType (typ lvl env exp), typ_of_rows lvl env rest)
    in
    TRec (typ_of_rows lvl env rows)

| SelectE (exp, id) ->
    let expty   = typ lvl env exp in
    let fieldty = TVar (new_typevar lvl) in
    unify lvl expty (TRec (Row (id, FieldType fieldty, Rho (new_recvar lvl []))));
    fieldty

| RecUpdE (exp, id, extexp) -> TInt (* TODO *)

(* staged computations *)
| ValueE _  | BoxE _  | UnboxE _  | RunE _  | LiftE _ as e ->
    raise (NotImplemented e)

let stdenv =
  let arith_op_ty = TypeScheme ([], TFun (TInt, TFun (TInt, TInt))) in
  let ref0 id = TVar (ref (NoLink id, 0)) in

  List.map (fun id -> (id, arith_op_ty)) [ "+"; "-"; "*"; "/" ]
    @ [ ("=", TypeScheme ( ["a"]
                         , TFun (ref0 "a", TFun ( ref0 "a" , TBool))))
      ; ("::", TypeScheme ( ["a"]
                          , TFun ( ref0 "a"
                                  , TFun ( TList (ref0 "a")
                                          , TList (ref0 "a")))))
      ; ("head", TypeScheme ( ["a"] , TFun ( TList (ref0 "a") , ref0 "a")))
      ; ("tail", TypeScheme ( ["a"] , TFun ( TList (ref0 "a") , TList (ref0 "a"))))
      ; ("empty", TypeScheme ( ["a"], TFun (TList (ref0 "a"), TBool)))
      ; ("nth", TypeScheme ( ["a"]
                           , TFun ( TInt , TFun ( TList (ref0 "a") , ref0 "a"))))
      ]


(* ---}}}---------------------------------------------------------------------*)
