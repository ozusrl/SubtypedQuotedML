open Common

(*  misc. --{{{-------------------------------------------------------------- *)

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
 | TUnit

 | TPair of ty * ty

 | TList of ty
 | TRef  of ty

 | TFun of ty * ty
 | TRec of tyrec
 | TVar of typevar

 | TBox of tyrec * ty
and 'a link =
 | NoLink of id
 | LinkTo of 'a
and field =
 | FieldType of ty
 | FieldVar of fieldvar
 | Bot
and 'a rhoMap =
 | EmptyRec
 | Rho of recvar
 | Row of id * 'a * 'a rhoMap

and tyrec = field rhoMap

and fieldScheme = Scheme of linkvar list * field

  (* TODO: maybe we should rename it with `tyscm` *)
and tyenv = fieldScheme rhoMap

and typevar = (tyvarlink * int) ref
and tyvarlink = ty link

and fieldvar = (fieldvarlink * int) ref
and fieldvarlink = field link

and recvar = (recvarlink * int * IdSet.t) ref
and recvarlink = tyrec link

and linkvar =
 | TV of typevar
 | FV of fieldvar
 | RV of recvar

(* ---}}}---------------------------------------------------------------------*)

(* link operations ---{{{-----------------------------------------------------*)

let linkvar_eq lv1 lv2 = match lv1, lv2 with
| TV tv1, TV tv2 -> tv1 = tv2
| FV fv1, FV fv2 -> fv1 = fv2
| RV rv1, RV rv2 ->
    begin match !rv1, !rv2 with
    | (link1, lvl1, _), (link2, lvl2, _) -> link1 = link2 && lvl1 = lvl2
    end
| _, _ -> false

let link_lvl (link : linkvar) : int = match link with
| TV typevar  -> let (_, i)    = !typevar in i
| FV fieldvar -> let (_, i)    = !fieldvar in i
| RV recvar   -> let (_, i, _) = !recvar in i

module IdGen' = IdGen.IdGen (struct end)
open IdGen'

let new_nolink _ = NoLink (new_name ())

let new_typevar (lvl : int) : typevar =
  ref (new_nolink (), lvl)

let new_recvar (lvl : int) (ids : IdSet.t) : recvar =
  ref (new_nolink (), lvl, ids)

let new_fieldvar (lvl : int) : fieldvar =
  ref (new_nolink (), lvl)

let set_link (typevar : typevar) (newlink : ty link) =
  let (_, lvl) = !typevar in
  typevar := (newlink, lvl)

let set_link_level tyvar newlvl =
  let (link, _) = !tyvar in
  tyvar := (link, newlvl)

let bottoms (recvar : recvar) : IdSet.t =
  let (_, _, btms) = !recvar in
  btms

let set_bottoms (recvar : recvar) (btmset : IdSet.t) : unit =
  let (id, lvl, _) = !recvar in
  recvar := (id, lvl, btmset)

(* ---}}}---------------------------------------------------------------------*)

(* type scheme instantiation ---{{{-------------------------------------------*)

let rec instantiate lvl (Scheme (tvs, fld)) : field =
  let ss = List.map (fun tv ->
    let lvl = link_lvl tv in
    (tv, new_typevar lvl, new_recvar lvl IdSet.empty, new_fieldvar lvl)) tvs in
  subst_field fld ss

and subst_field field ss = match field, ss with
  | _, [] -> field
  | FieldType ty, ss -> FieldType (subst ty ss)
  | Bot, _ -> Bot
  | FieldVar fieldvar, ((l, _, _, t) :: rest) ->
      if linkvar_eq (FV fieldvar) l then FieldVar t
      else subst_field field rest

and subst_tyrec tyrec ss = match tyrec, ss with
  | _, [] -> tyrec
  | EmptyRec, _ -> EmptyRec
  | Rho recvar, ((l, _, t, _) :: rest) ->
      if linkvar_eq (RV recvar) l then
        begin
          let btmset = bottoms recvar in
          set_bottoms t btmset;
          Rho t
        end
      else subst_tyrec tyrec rest
  | Row (id, field, tyrec), ss ->
      Row (id, subst_field field ss, subst_tyrec tyrec ss)

and subst ty ss = match ty, ss with
  | _, [] -> ty
  | TInt, _  -> TInt
  | TBool, _ -> TBool
  | TUnit, _ -> TUnit
  | TPair (t1, t2), ss -> TPair (subst t1 ss, subst t2 ss)
  | TList t, ss -> TList (subst t ss)
  | TRef t, ss -> TRef (subst t ss)
  | TFun (t1, t2), ss -> TFun (subst t1 ss, subst t2 ss)
  | TRec tyrec, ss -> TRec (subst_tyrec tyrec ss)
  | TVar typevar, (l, t, _, _) :: rest ->
      begin match !typevar with
      | NoLink _, _ -> if linkvar_eq (TV typevar) l then TVar t else subst ty rest
      | LinkTo ty, _ -> subst ty ss
      end
  | TBox (tyrec, ty), ss -> TBox (subst_tyrec tyrec ss, subst ty ss)

let rec instantiate_env lvl env : tyrec =
  match env with
  | EmptyRec -> EmptyRec
  | Rho recvar -> Rho recvar
  | Row (id, fldscm, rest) -> Row (id, instantiate lvl fldscm, instantiate_env lvl rest)


(* ---}}}---------------------------------------------------------------------*)

(* norm ty ---{{{-------------------------------------------------------------*)

let rec norm_ty = function
| TVar typevar ->
    begin match !typevar with
    | LinkTo t1, _ ->
        let t2 = norm_ty t1 in
        set_link typevar (LinkTo t2);
        t2
    | _ -> TVar typevar
    end
| t -> t

let rec norm_field = function
| FieldType ty -> FieldType ty
| FieldVar fieldvar ->
    begin match !fieldvar with
    | LinkTo fv1, lvl ->
        let fv1' = norm_field fv1 in
        fieldvar := (LinkTo fv1', lvl);
        fv1'
    | _ -> FieldVar fieldvar
    end
| Bot -> Bot

let rec norm_tyrec (rho : 'a rhoMap) : 'a rhoMap =
  let rec mkRec fields rho = match fields with
  | [] -> rho
  | ((id, a) :: rest) -> Row (id, a, mkRec rest rho)
  in

  let rec iter fields = function
  | EmptyRec -> mkRec fields EmptyRec
  | Row (id, a, next) -> iter ((id, norm_field a) :: fields) next
  | Rho recvar ->
      begin match !recvar with
      | NoLink _, _, _ -> mkRec fields (Rho recvar)
      | LinkTo recvar', _, _ -> iter fields recvar'
      end
  in

  iter [] rho

(* ---}}}---------------------------------------------------------------------*)

(* freetyvars ---{{{----------------------------------------------------------*)

let rec freetyvars (ty : ty) : linkvar list =
  match norm_ty ty with
  | TUnit
  | TBool
  | TInt  -> []
  | TPair (t1, t2) -> unique (freetyvars t1 @ freetyvars t2)
  | TList t
  | TRef  t -> freetyvars t
  | TFun (t1, t2) -> unique (freetyvars t1 @ freetyvars t2)
  | TRec tyrec    -> freetyvars_tyrec tyrec
  | TVar typevar  -> [TV typevar] (* typevar must be a NoLink *)
  | TBox (tyrec, ty) -> freetyvars_tyrec tyrec @ freetyvars ty

and freetyvars_tyrec tyrec =
  match norm_tyrec tyrec with
  | EmptyRec -> []
  | Rho recvar -> [RV recvar] (* recvar must be a NoLink *)
  | Row (_, fld, tyrec) -> freetyvars_field fld @ freetyvars_tyrec tyrec

and freetyvars_field fld =
  match norm_field fld with
  | FieldType t -> freetyvars t
  | Bot -> []
  | FieldVar fv -> [FV fv]

(* ---}}}---------------------------------------------------------------------*)

(* unification --{{{----------------------------------------------------------*)

let link_typevar_to_ty (typevar : typevar) (ty : ty) : unit =
  let occur_check (typevar : typevar) (links : linkvar list) : bool =
    let freevars = freetyvars (TVar typevar) in
    let bs = List.map (fun var -> List.mem var links) freevars in
    List.fold_right (fun a b -> a or b) bs false
  in

  let prune (maxlvl : int) (tvs : linkvar list) : unit =
    let reducelvl (tvar : linkvar) = match tvar with
    | TV typevar ->
        let (_, lvl) = !typevar in
        set_link_level typevar (min lvl maxlvl)
    | _ -> ()
    in
    List.iter reducelvl tvs
  in

  let (_, level) = !typevar in
  let fvs = freetyvars ty in
  if occur_check typevar fvs then
    failwith "type error: circularity"
  else
    prune level fvs;
    set_link typevar (LinkTo ty)

let rec field_set tyrec = match norm_tyrec tyrec with
| EmptyRec -> IdSet.empty
| Rho recvar -> IdSet.empty
| Row (id, field, rest) -> IdSet.add id (field_set rest)

let rec get_field_ty id tyrec = match norm_tyrec tyrec with
| EmptyRec
| Rho _ -> assert false
| Row (id', ty, rest) -> if id = id' then norm_field ty else get_field_ty id rest

let rec get_row_var tyrec = match norm_tyrec tyrec with
| EmptyRec -> None
| Rho row -> Some row
| Row (_, _, rest) -> get_row_var rest

let link_fieldvar_to_field (fieldvar : fieldvar) (field : field) =
  let (link, lvl) = !fieldvar in
  fieldvar := (LinkTo field, lvl)

let link_recvar_to_tyrec (recvar : recvar) (tyrec : tyrec) =
  let (link, lvl, ids) = !recvar in
  recvar := (LinkTo tyrec, lvl, ids)

let rec unify_fields f1 f2 = match norm_field f1, norm_field f2 with
| Bot, Bot -> ()
| FieldType t1, FieldType t2 -> unify t1 t2
| FieldVar fieldvar1, FieldVar fieldvar2 ->
    if fieldvar1 = fieldvar2 then ()
    else
      let (_, fv1level) = !fieldvar1 in
      let (_, fv2level) = !fieldvar2 in
      if fv1level < fv2level then
        link_fieldvar_to_field fieldvar1 f2
      else
        link_fieldvar_to_field fieldvar2 f1
| FieldVar fieldvar, ty
| ty, FieldVar fieldvar -> link_fieldvar_to_field fieldvar ty

| _, _ -> failwith"can't unify fields"

and unify_recs tyrec1 tyrec2 =
  match norm_tyrec tyrec1, norm_tyrec tyrec2 with
| EmptyRec, EmptyRec -> ()
| Rho link1, Rho link2 ->
    if link1 = link2 then ()
    else
      begin
        let (link1', lvl1, btms1) = !link1 in
        let (link2', lvl2, btms2) = !link2 in
        let btms = IdSet.union btms1 btms2 in
        link1 := (link1', lvl1, btms);
        link2 := (link2', lvl2, btms);
        if lvl1 < lvl2 then
          link_recvar_to_tyrec link1 tyrec2
        else
          link_recvar_to_tyrec link2 tyrec1
      end
| EmptyRec, Rho link
| Rho link, EmptyRec ->
    begin match !link with
    | NoLink _, _, _ ->
        link_recvar_to_tyrec link EmptyRec
    | LinkTo r, _, _ -> failwith "LinkTo not expected for Rho due to normalization."
    end
| _, _ ->
    let field_set_1 = field_set tyrec1 in
    let field_set_2 = field_set tyrec2 in

    let inter = IdSet.inter field_set_1 field_set_2 in
    let diff1 = IdSet.diff field_set_1 field_set_2 in
    let diff2 = IdSet.diff field_set_2 field_set_1 in

    IdSet.iter (fun e ->
      let f1 = get_field_ty e tyrec1 in
      let f2 = get_field_ty e tyrec2 in
      unify_fields f1 f2) inter;

    let add_fields_to_rho (ids : IdSet.t) tyrec target_tyrec =
      IdSet.iter (fun e ->
        let option_recvar = get_row_var target_tyrec in
        match option_recvar with
        | None ->
            begin match get_field_ty e tyrec with
            | FieldType ty -> failwith ("can't unify field " ^ e ^ " with Bot")
            | FieldVar fieldvar -> link_fieldvar_to_field fieldvar Bot
            | Bot -> ()
            end
        | Some rho ->
            let (NoLink id, lvl, bottoms) = !rho in
            if IdSet.mem e bottoms then
              begin match get_field_ty e tyrec with
              | FieldType _ -> failwith ("can't unify field " ^ e ^ " with Bot")
              | FieldVar fieldvar -> link_fieldvar_to_field fieldvar Bot
              | Bot -> ()
              end
            else
              let new_rho_id = new_name () in
              let ty = get_field_ty e tyrec in
              let new_rho = Rho (ref (NoLink new_rho_id, lvl, bottoms)) in
              rho := (LinkTo (Row (e, ty, new_rho)), lvl, bottoms)) ids
    in

    add_fields_to_rho diff1 tyrec1 tyrec2;
    add_fields_to_rho diff2 tyrec2 tyrec1;

    let row1 = get_row_var tyrec1 in
    let row2 = get_row_var tyrec2 in

    match row1, row2 with
    | None, None -> ()
    | Some rho, None
    | None, Some rho -> link_recvar_to_tyrec rho EmptyRec
    | Some rho1, Some rho2 -> unify_recs (Rho rho1) (Rho rho2)

and unify (t1 : ty) (t2 : ty) : unit =
  let t1' = norm_ty t1 in
  let t2' = norm_ty t2 in
  match (t1', t2') with
  | TInt,  TInt
  | TBool, TBool
  | TUnit, TUnit -> ()
  | TPair (t1, t2), TPair (t1', t2') -> unify t1 t1'; unify t2 t2'
  | TList ty1, TList ty2 -> unify ty1 ty2
  | TRef ty1,  TRef ty2  -> unify ty1 ty2
  | TFun (t1, t2), TFun (t1', t2') -> unify t1 t1'; unify t2 t2'
  | TRec tyrec1, TRec tyrec2 -> unify_recs tyrec1 tyrec2

  | TVar typevar1, TVar typevar2 ->
      if typevar1 = typevar2 then ()
      else
        let (_, tv1level) = !typevar1 in
        let (_, tv2level) = !typevar2 in
        if tv1level < tv2level then
          link_typevar_to_ty typevar1 t2'
        else
          link_typevar_to_ty typevar2 t1'

  | TVar typevar, ty
  | ty, TVar typevar ->
      link_typevar_to_ty typevar ty

  | TBox (gamma1, t1), TBox (gamma2, t2) -> unify_recs gamma1 gamma2; unify t1 t2
  | t1, t2 -> failwith "can't unify types"

(* ---}}}---------------------------------------------------------------------*)

(* type inference --{{{-------------------------------------------------------*)

let generalize lvl (fld : field) : fieldScheme =
  let notfreeincontext link =
    link_lvl link > lvl
  in
  let tvs = List.filter notfreeincontext (unique (freetyvars_field fld)) in
  Scheme (tvs, fld)

let add_to_envlist lvl id (scm : fieldScheme) (envs : tyenv list) =
  let rec add_or_replace id (scm : fieldScheme) : tyenv -> tyenv = function
  | EmptyRec -> Row (id, scm, EmptyRec)
  | Rho recvar ->
      let theta, newrho = new_fieldvar lvl, new_recvar lvl (IdSet.singleton id) in
      unify_recs (Rho recvar) (Row (id, FieldVar theta, Rho newrho));
      (Row (id, scm, Rho newrho))
  | Row (id', scm', next) ->
      if id' = id then
        Row (id, scm, next)
      else
        Row (id', scm', add_or_replace id scm next)
  in
  add_or_replace id scm (List.hd envs) :: List.tl envs

let inc_level lvls = (List.hd lvls + 1) :: List.tl lvls

let rec typ (lvls : int list) (envs : tyenv list) : (exp -> ty) =
  let lvl = List.hd lvls in
  function
| IdE id ->
    (try
      let scm = List.assoc id stdenv in
      let (FieldType ty) = instantiate lvl scm in
      ty
    with Not_found ->
      let gamma = instantiate_env lvl (List.hd envs) in
      let alpha = TVar (new_typevar lvl) in
      let rho = Rho (new_recvar lvl (IdSet.singleton id)) in
      unify (TRec (Row (id, FieldType alpha, rho))) (TRec gamma);
      alpha)
| ConstE (CInt _) -> TInt
| ConstE (CBool _) -> TBool
| EmpLstE -> TList (TVar (new_typevar lvl))
| PairE (e1, e2) ->
    let t1 = typ lvls envs e1 in
    let t2 = typ lvls envs e2 in
    TPair (t1, t2)
| AppE (e1, e2) ->
    let funty = typ lvls envs e1 in
    let argty = typ lvls envs e2 in
    let retty = TVar (new_typevar lvl) in
    unify funty (TFun (argty, retty));
    retty
| AbsE (Abs (id, body)) ->
    let ptyv       = TVar (new_typevar lvl) in (* parameter type *)
    let f_body_env = add_to_envlist lvl id (Scheme ([], FieldType ptyv)) envs in
    let rtyp       = typ lvls f_body_env body in (* return value type *)
    TFun (ptyv, rtyp)
| LetInE (Valbind (id, rhs), body) ->
    let rhsty  = typ (inc_level lvls) envs rhs in
    let is_expansive = expansive lvl rhs in
    let rhs_tyscm =
      if is_expansive then
        Scheme ([], FieldType rhsty)
      else
        generalize lvl (FieldType rhsty)
    in
    let letenv = add_to_envlist lvl id rhs_tyscm envs in
    typ lvls letenv body
| FixE (fname, Abs (id, body)) ->
    let ptyv       = TVar (new_typevar lvl) in
    let rtyv       = TVar (new_typevar lvl) in
    let f_body_env =
      add_to_envlist lvl id (Scheme ([], FieldType ptyv))
        (add_to_envlist lvl fname (Scheme ([], FieldType (TFun (ptyv, rtyv)))) envs)
    in
    let rtyp = typ lvls f_body_env body in
    unify rtyp rtyv;
    TFun (ptyv, rtyv)

| IfE (guard, thenE, elseE) ->
    let guard_ty = typ lvls envs guard in
    unify guard_ty TBool;
    let then_ty = typ lvls envs thenE in
    let else_ty = typ lvls envs elseE in
    unify then_ty else_ty;
    then_ty

| SeqE (e1, e2) ->
    let _ = typ lvls envs e1 in
    typ lvls envs e2

(* refs -------------------------------------------*)
| RefE e -> TRef (typ lvls envs e)
| DerefE e ->
    let ref_ty = typ lvls envs e in
    let ret = TVar (new_typevar lvl) in
    unify ref_ty (TRef ret);
    ret
| AssignE (e1, e2) ->
    let ref_ty = typ lvls envs e1 in
    let new_val_ty = typ lvls envs e2 in
    unify ref_ty (TRef new_val_ty);
    TUnit

(* records --------------------------------------- *)
| EmptyRecE -> TRec EmptyRec

| SelectE (exp, id) ->
    let rho = Rho (new_recvar lvl (IdSet.singleton id)) in
    let retty = TVar (new_typevar lvl) in

    let expty = typ lvls envs exp in
    unify expty (TRec (Row (id, FieldType retty, rho)));
    retty

| RecUpdE (exp, id, extexp) ->
    let rho = Rho (new_recvar lvl (IdSet.singleton id)) in
    let expty  = typ lvls envs exp in
    unify expty (TRec (Row (id, FieldVar (new_fieldvar lvl), rho)));

    let extty = typ lvls envs extexp in
    TRec (Row (id, FieldType extty, rho))

(* staged computations *)
| BoxE exp ->
    let newrho = new_recvar lvl IdSet.empty in
    let expty  = typ (0 :: lvls) (Rho newrho :: envs) exp in
    TBox (Rho newrho, expty)

| RunE exp ->
    let expty = typ lvls envs exp in
    let retty = TVar (new_typevar lvl) in
    unify (TBox (EmptyRec, retty)) expty;
    retty

| LiftE exp ->
    let expty = typ lvls envs exp in
    TBox (Rho (new_recvar lvl IdSet.empty), expty)

| UnboxE exp ->
    let expty = typ (List.tl lvls) (List.tl envs) exp in
    let gamma = instantiate_env lvl (List.hd envs) in
    let alpha = TVar (new_typevar lvl) in
    unify expty (TBox (gamma, alpha));
    alpha

| ValueE _ as e ->
    raise (NotImplemented e)

and stdenv =
  let arith_op_ty = Scheme ([], FieldType (TFun (TInt, TFun (TInt, TInt)))) in
  let ref0 id = TVar (ref (NoLink id, 0)) in
  let tv id = TV (ref (NoLink id, 0)) in
  let rv id = RV (ref (NoLink id, 0, IdSet.empty)) in

  List.map (fun id -> (id, arith_op_ty)) [ "+"; "-"; "*"; "/" ]
    @ [ ("=",  Scheme ( [tv "a"]
                      , FieldType (TFun (ref0 "a", TFun (ref0 "a", TBool)))))
      ; ("::", Scheme ( [tv "a"]
                      , FieldType ( TFun ( ref0 "a"
                                         , TFun ( TList (ref0 "a")
                                                , TList (ref0 "a"))))))
      ; ("head",  Scheme ([tv "a"], FieldType (TFun (TList (ref0 "a"), ref0 "a"))))
      ; ("tail",  Scheme ([tv "a"], FieldType (TFun (TList (ref0 "a"), TList (ref0 "a")))))
      ; ("empty", Scheme ([tv "a"], FieldType (TFun (TList (ref0 "a"), TBool))))
      ; ("nth",   Scheme ( [tv "a"]
                         , FieldType (TFun (TInt, TFun (TList (ref0 "a"), ref0 "a")))))
      ; ("fst",   Scheme ( [tv "a"; tv "b"]
                         , FieldType (TFun (TPair (ref0 "a", ref0 "b"), ref0 "a"))))
      ; ("snd",   Scheme ( [tv "a"; tv "b"]
                         , FieldType (TFun (TPair (ref0 "a", ref0 "b"), ref0 "b"))))
      ; ("_force_record", Scheme ([rv "a"], FieldType (TFun (TRec (Rho (ref (NoLink "a", 0, IdSet.empty))), TUnit))))
      ; ("force_nonrecord", Scheme ([rv "a"], FieldType (TFun (ref0 "a", TUnit))))
      ]

let stdenv_tyrec =
  let rec iter = function
  | [] -> EmptyRec
  | (id, scm) :: rest -> Row (id, scm, iter rest)
  in
  iter stdenv

(* ---}}}---------------------------------------------------------------------*)

