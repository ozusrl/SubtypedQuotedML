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

and recvar = (recvarlink * int * IdSet.t) ref
and recvarlink = tyrec link

type linkvar =
  | TV of typevar
  | FV of fieldvar
  | RV of recvar

type tyscm = TypeScheme of linkvar list * ty

type tenv = (id * tyscm) list

(* ---}}}---------------------------------------------------------------------*)

(* link operations ---{{{-----------------------------------------------------*)

let linkvar_eq lv1 lv2 = match lv1, lv2 with
| TV tv1, TV tv2 -> tv1 = tv2
| FV fv1, FV fv2 -> fv1 = fv2
| RV rv1, RV rv2 -> (match !rv1, !rv2 with
  | (link1, lvl1, _), (link2, lvl2, _) -> link1 = link2 && lvl1 = lvl2)
| _, _ -> false

let link_lvl (link : linkvar) : int = match link with
| TV typevar  -> let (_, i)    = !typevar in i
| FV fieldvar -> let (_, i)    = !fieldvar in i
| RV recvar   -> let (_, i, _) = !recvar in i

let last_tyvar = ref 0
let new_name _ =
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
  int_to_name !last_tyvar

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

(* ---}}}---------------------------------------------------------------------*)

(* type scheme instantiation ---{{{-------------------------------------------*)

let rec instantiate lvl (TypeScheme (tvs, ty)) : ty =
  let ss = List.map (fun tv ->
    let lvl = link_lvl tv in
    (tv, new_typevar lvl, new_recvar lvl IdSet.empty, new_fieldvar lvl)) tvs in

  let rec subst_field field ss = match field, ss with
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
      if linkvar_eq (RV recvar) l then Rho t
      else subst_tyrec tyrec rest
  | Row (id, field, tyrec), ss ->
      Row (id, subst_field field ss, subst_tyrec tyrec ss)

  and subst ty ss = match ty, ss with
  | _, [] -> ty
  | TInt, _ -> TInt
  | TBool, _ -> TBool
  | TList t, ss -> TList (subst t ss)
  | TRef t, ss -> TRef (subst t ss)
  | TFun (t1, t2), ss -> TFun (subst t1 ss, subst t2 ss)
  | TRec tyrec, ss -> TRec (subst_tyrec tyrec ss)
  | TVar typevar, (l, t, _, _) :: rest -> (match !typevar with
    | NoLink _, _ -> if linkvar_eq (TV typevar) l then TVar t else subst ty rest
    | LinkTo ty, _ -> subst ty ss)
  in
  subst ty ss

(* ---}}}---------------------------------------------------------------------*)

(* TODO: Prune the levels on-the-fly, during linking *)

(* norm ty ---{{{-------------------------------------------------------------*)

let rec norm_ty = function
| TVar typevar -> (match !typevar with
  | LinkTo t1, _ ->
      let t2 = norm_ty t1 in
      set_link typevar (LinkTo t2);
      t2
  | _ -> TVar typevar)
| t -> t

let rec norm_tyrec = function
| EmptyRec -> EmptyRec
| Rho recvar -> (match !recvar with
  | LinkTo r1, lvl, ids ->
      let r1' = norm_tyrec r1 in
      recvar := (LinkTo r1', lvl, ids);
      r1'
  | _ -> Rho recvar)
| t -> t

let rec norm_field = function
| FieldType ty -> FieldType ty
| FieldVar fieldvar -> (match !fieldvar with
  | LinkTo fv1, lvl ->
      let fv1' = norm_field fv1 in
      fieldvar := (LinkTo fv1', lvl);
      fv1'
  | _ -> FieldVar fieldvar)
| Bot -> Bot

(* ---}}}---------------------------------------------------------------------*)

(* freetyvars ---{{{----------------------------------------------------------*)

let rec freetyvars (ty : ty) : linkvar list =
  let rec freetyvars_tyrec tyrec = match norm_tyrec tyrec with
  | EmptyRec -> []
  | Rho recvar -> [RV recvar] (* recvar must be a NoLink *)
  | Row (_, field, tyrec) ->
      (match norm_field field with
      | FieldType t -> freetyvars t @ freetyvars_tyrec tyrec
      | Bot -> freetyvars_tyrec tyrec
      | FieldVar fv -> FV fv :: freetyvars_tyrec tyrec)
  in
  match norm_ty ty with
  | TInt  -> []
  | TBool -> []
  | TList t
  | TRef  t -> freetyvars t
  | TFun (t1, t2) -> unique(freetyvars t1 @ freetyvars t2)
  | TRec tyrec    -> freetyvars_tyrec tyrec
  | TVar typevar  -> [TV typevar] (* typevar must be a NoLink *)

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
| Row (id', ty, rest) -> if id = id' then ty else get_field_ty id rest

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
| Rho link, EmptyRec -> (match !link with
  | NoLink _, _, _ -> link_recvar_to_tyrec link EmptyRec
  | LinkTo r, _, _ -> failwith "LinkTo not expected for Rho due to normalization.")
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
            (match get_field_ty e tyrec with
            | FieldType ty -> failwith ("can't unify field " ^ e ^ " with Bot")
            | FieldVar fieldvar -> link_fieldvar_to_field fieldvar Bot
            | Bot -> ())
        | Some rho ->
            let (NoLink id, lvl, bottoms) = !rho in
            if IdSet.mem e bottoms then
              begin
                match get_field_ty e tyrec with
                | FieldType _ -> failwith "can't unify records -- 1"
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

  | TUnit, TUnit
  | TInt,  TInt
  | TBool, TBool -> ()
  | TList ty1, TList ty2 -> unify ty1 ty2
  | TRef ty1,  TRef ty2  -> unify ty1 ty2
  | TFun (a, b), TFun (c, d) -> unify a c; unify b d
  | TRec tyrec1, TRec tyrec2 -> unify_recs tyrec1 tyrec2

  | _, _ -> failwith "can't unify types"

(* ---}}}---------------------------------------------------------------------*)

(* type inference --{{{-------------------------------------------------------*)

let rec generalize lvl (t : ty) : tyscm =
  let notfreeincontext link =
    link_lvl link > lvl
  in
  let tvs = List.filter notfreeincontext (unique (freetyvars t)) in
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
    unify funty (TFun (argty, retty));
    retty
| AbsE (Abs (id, body)) ->
    let ptyv       = TVar (new_typevar lvl) in (* parameter type *)
    let f_body_env = (id, TypeScheme([], ptyv)) :: env in
    let rtyp       = typ lvl f_body_env body in (* return value type *)
    TFun (ptyv, rtyp)
| LetInE (Valbind (id, rhs), body) ->
    let lvl' = lvl + 1 in
    let rhsty  = typ lvl' env rhs in
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
    unify rtyp rtyv;
    TFun (ptyv, rtyv)
| CondE [] -> failwith "CondE with empty cond list"
| CondE ((guard, body) :: rest) ->
    let rec iter body_ty = function
    | [] -> body_ty
    | (guard, body) :: rest ->
        let guard_ty = typ lvl env guard in
        unify guard_ty TBool;
        let body_ty' = typ lvl env body in
        unify body_ty body_ty';
        iter body_ty rest
    in
    let guard_ty = typ lvl env guard in
    unify guard_ty TBool;
    let body_ty = typ lvl env body in
    iter body_ty rest

(* refs -------------------------------------------*)
| RefE e -> TRef (typ lvl env e)
| DerefE e ->
    let ref_ty = typ lvl env e in
    let ret = TVar (new_typevar lvl) in
    unify ref_ty (TRef ret);
    ret
| AssignE (e1, e2) ->
    let ref_ty = typ lvl env e1 in
    let new_val_ty = typ lvl env e2 in
    unify ref_ty (TRef new_val_ty);
    TUnit

(* records --------------------------------------- *)
| EmptyRecE -> TRec EmptyRec

| SelectE (exp, id) ->
    let rho = Rho (new_recvar lvl (IdSet.singleton id)) in
    let retty = TVar (new_typevar lvl) in

    let expty = typ lvl env exp in
    unify expty (TRec (Row (id, FieldType retty, rho)));
    retty

| RecUpdE (exp, id, extexp) ->
    let rho = Rho (new_recvar lvl (IdSet.singleton id)) in
    let expty  = typ lvl env exp in
    unify expty (TRec (Row (id, FieldVar (new_fieldvar lvl), rho)));

    let extty = typ lvl env extexp in
    TRec (Row (id, FieldType extty, rho))

(* staged computations *)
| ValueE _  | BoxE _  | UnboxE _  | RunE _  | LiftE _ as e ->
    raise (NotImplemented e)

let stdenv =
  let arith_op_ty = TypeScheme ([], TFun (TInt, TFun (TInt, TInt))) in
  let ref0 id = TVar (ref (NoLink id, 0)) in
  let tv id = TV (ref (NoLink id, 0)) in

  List.map (fun id -> (id, arith_op_ty)) [ "+"; "-"; "*"; "/" ]
    @ [ ("=", TypeScheme ( [tv "a"]
                         , TFun (ref0 "a", TFun (ref0 "a" , TBool))))
      ; ("::", TypeScheme ( [tv "a"]
                          , TFun ( ref0 "a"
                                  , TFun ( TList (ref0 "a")
                                         , TList (ref0 "a")))))
      ; ("head", TypeScheme ([tv "a"] , TFun ( TList (ref0 "a") , ref0 "a")))
      ; ("tail", TypeScheme ([tv "a"] , TFun ( TList (ref0 "a") , TList (ref0 "a"))))
      ; ("empty", TypeScheme ([tv "a"], TFun (TList (ref0 "a"), TBool)))
      ; ("nth", TypeScheme ( [tv "a"]
                           , TFun ( TInt , TFun ( TList (ref0 "a") , ref0 "a"))))
      ]

(* ---}}}---------------------------------------------------------------------*)

