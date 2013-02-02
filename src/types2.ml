open Common
open Sexplib.Sexp
open Sexplib.Conv

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

type ty =
  | IntTy
  | BoolTy
  | FunTy of (ty * ty)
  | VarTy of typevar
  | ListTy of ty
  | RecTy of (field list * rowvar option)

and typevar = (tyvarkind * int) ref

and tyvarkind =
  | NoLink of string  (* just a type variable *)
  | LinkTo of ty      (* equated to type ty *)

and field = (id * ty)

and rowvar = typevar
  with sexp

type tyscm = TypeScheme of (typevar list * ty) with sexp

type tenv = (id * tyscm) list with sexp

let show_type ty = to_string (sexp_of_ty ty)

let tyvarno = ref 0

let set_tv_kind tyvar newkind =
  let (kind, lvl) = !tyvar in
  tyvar := (newkind, lvl)

let set_tv_level tyvar newlvl =
  let (kind, _) = !tyvar in
  tyvar := (kind, newlvl)

let new_typevar (lvl : int) : typevar =
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
  tyvarno := !tyvarno + 1;
  ref (NoLink (int_to_name !tyvarno), lvl)

let instantiate lvl (TypeScheme (tvs, t)) : ty =
  if List.length tvs = 0 then
    t
  else
    let ss = List.map (fun tv -> (tv, VarTy (new_typevar lvl))) tvs in
    let rec subst ty ss = match ty, ss with
    | ty, [] -> ty
    | IntTy, _ -> IntTy
    | BoolTy, _ -> BoolTy
    | FunTy (ty1, ty2), ss -> FunTy (subst ty1 ss, subst ty2 ss)
    | VarTy link, ((l, t) :: rest) ->
        if link = l then t else subst ty rest
    | ListTy ty, ss -> ListTy (subst ty ss)
    | RecTy (fields, rowvar), ss ->
        let rec inst_fields = function
        | [] -> []
        | (id, ty) :: rest -> (id, subst ty ss) :: inst_fields rest
        in
        match rowvar with
        | None -> RecTy (inst_fields fields, None)
        | Some row ->
            let (VarTy instrow) = subst (VarTy row) ss in
            RecTy (inst_fields fields, Some instrow)
    in
    subst t ss

let set_tvkind tyvar newkind =
  let (kind, lvl) = !tyvar in
  tyvar := (newkind, lvl)

let rec norm_ty = function
| VarTy tyvar -> (match !tyvar with
  | LinkTo t1, _ ->
      let t2 = norm_ty t1 in
      set_tvkind tyvar (LinkTo t2);
      t2
  | _ -> VarTy tyvar)
| t -> t

(*let rec norm_ty = function
| IntTy -> IntTy
| BoolTy -> BoolTy
| FunTy (t1, t2) -> FunTy (norm_ty t1, norm_ty t2)
| VarTy tyvar -> (match !tyvar with
  | LinkTo t1, _ ->
      let t2 = norm_ty t1 in
      set_tvkind tyvar (LinkTo t2);
      t2
  | _ -> VarTy tyvar)
| ListTy t -> ListTy (norm_ty t)*)

let rec freetyvars t : typevar list = match norm_ty t with
| IntTy          -> []
| BoolTy         -> []
| FunTy (t1, t2) -> union (freetyvars t1) (freetyvars t2)
| VarTy tv       -> [tv]
| ListTy t1      -> freetyvars t1
| RecTy (fields, rowvar) ->
    let rec tyvars_of_fields = function
    | [] -> []
    | (_, ty) :: rest -> freetyvars ty @ tyvars_of_fields rest
    in
    match rowvar with
    | Some typevar -> tyvars_of_fields fields @ freetyvars (VarTy typevar)
    | None -> tyvars_of_fields fields

let rec generalize lvl (t : ty) : tyscm =
  let notfreeincontext tyvar =
    let (_, linklvl) = !tyvar in
    linklvl > lvl
  in
  let tvs = unique (List.filter notfreeincontext (freetyvars t)) in
  TypeScheme (tvs, t)

let rec link_to_ty (tyvar : typevar) (t : ty) =
  let occur_check = List.mem in

  let prune maxlvl tvs =
    let reducelvl tyvar =
      let (_, lvl) = !tyvar in
      set_tv_level tyvar (min lvl maxlvl)
    in
    List.iter reducelvl tvs
  in

  let (_, level) = !tyvar in
  let fvs = freetyvars t in
  if occur_check tyvar fvs then
    failwith "type error: circularity"
  else
    prune level fvs;
    set_tv_kind tyvar (LinkTo t)

let field_sort fields =
  let sort_fn (id1, _) (id2, _) = compare id1 id2 in
  List.sort sort_fn fields

let rec unify_recs_concrete lvl fields1 fields2 =
  match field_sort fields1, field_sort fields2 with
  | [], [] -> ()
  | [], (id, _) :: _
  | (id, _) :: _, [] ->
      failwith (Printf.sprintf "can't unify record field %s." id)
  | (id1, ty1) :: r1, (id2, ty2) :: r2 ->
      if id1 = id2 then
        let _ = unify lvl ty1 ty2 in
        unify_recs_concrete lvl r1 r2
      else
        failwith (Printf.sprintf "can't unify record fields %s and %s." id1 id2)

and unify_recs_one_row lvl fields1 row fields2 =
  match field_sort fields1, field_sort fields2 with
  | [], [] -> ()
  | (id1, ty1) :: _, [] ->
      failwith (Printf.sprintf "can't unify record field %s." id1)
  | [], (id2, ty2) :: r2 ->
      unify lvl row (RecTy ([id2, ty2], Some (new_typevar lvl)));
      unify_recs_one_row lvl [] row r2
  | (id1, ty1) :: r1, (id2, ty2) :: r2 ->
      (match compare id1 id2 with
      | -1 ->
          failwith (Printf.sprintf "unify_recs_one_row: can't unify id %s." id1)
      |  0 ->
          unify lvl ty1 ty2; unify_recs_one_row lvl r1 row r2
      |  1 ->
          unify lvl row (RecTy ([(id2, ty2)], Some (new_typevar lvl)));
          unify_recs_one_row lvl ((id1, ty1) :: r1) row r2
      |  _ -> failwith"")

and unify_recs_two_row lvl fields1 row1 fields2 row2 =
  match field_sort fields1, field_sort fields2 with
  | [], [] -> ()
  | (id1, ty1) :: r1, [] ->
      unify lvl row2 (RecTy ([id1, ty1], Some (new_typevar lvl)));
      unify_recs_two_row lvl r1 row1 [] row2
  | [], (id2, ty2) :: r2 ->
      unify lvl row1 (RecTy ([id2, ty2], Some (new_typevar lvl)));
  | (id1, ty1) :: r1, (id2, ty2) :: r2 ->
      (match compare id1 id2 with
      | -1 ->
          unify lvl row2 (RecTy ([(id1, ty1)], Some (new_typevar lvl)));
          unify_recs_two_row lvl r1 row1 ((id2, ty2) :: r2) row2
      |  0 ->
          unify lvl ty1 ty2;
          unify_recs_two_row lvl r1 row1 r2 row2
      |  1 ->
          unify lvl row1 (RecTy ([(id2, ty2)], Some (new_typevar lvl)));
          unify_recs_two_row lvl ((id1, ty1) :: r1) row1 r2 row2)

and unify (lvl : int) (t1 : ty) (t2 : ty) : unit =
  let t1' = norm_ty t1 in
  let t2' = norm_ty t2 in
  match (t1', t2') with
  | IntTy, IntTy -> ()
  | BoolTy, BoolTy -> ()
  | FunTy (t1', t2'), FunTy (t1'', t2'') ->
      unify lvl t1' t1''; unify lvl t2' t2''
  | VarTy tv1, VarTy tv2 ->
      if tv1 = tv2 then ()
      else
        let (_, tv1level) = !tv1 in
        let (_, tv2level) = !tv2 in
        if tv1level < tv2level then
          link_to_ty tv1 t2'
        else
          link_to_ty tv2 t1'
  | ListTy tv1, ListTy tv2 -> unify lvl tv1 tv2
  | VarTy tv1, _ -> link_to_ty tv1 t2'
  | _, VarTy tv2 -> link_to_ty tv2 t1'

  | RecTy (fields1, None), RecTy (fields2, None) ->
      unify_recs_concrete lvl fields1 fields2
  | RecTy (fields1, Some var1), RecTy (fields2, None) ->
      unify_recs_one_row lvl fields1 (VarTy var1) fields2
  | RecTy (fields1, None), RecTy (fields2, Some var2) ->
      unify_recs_one_row lvl fields2 (VarTy var2) fields1
  | RecTy (fields1, Some var1), RecTy (fields2, Some var2) ->
      unify_recs_two_row lvl fields1 (VarTy var1) fields2 (VarTy var2)

  | t1, t2 -> failwith (Printf.sprintf "type error: %s and %s."
                  (show_type t1) (show_type t2))

let rec typ (lvl : int) (env : tenv) : (exp -> ty) = function
| IdE id -> instantiate lvl (List.assoc id env)
| ConstE (CInt _) -> IntTy
| ConstE (CBool _) -> BoolTy
| EmpLstE -> ListTy (VarTy (new_typevar lvl))
| AppE (e1, e2) ->
    let funty = typ lvl env e1 in
    let argty = typ lvl env e2 in
    let retty = VarTy (new_typevar lvl) in
    unify lvl funty (FunTy (argty, retty));
    retty
| AbsE (Abs (id, body)) ->
    let ptyv       = VarTy (new_typevar lvl) in (* parameter type *)
    let f_body_env = (id, TypeScheme([], ptyv)) :: env in
    let rtyp       = typ lvl f_body_env body in (* return value type *)
    FunTy (ptyv, rtyp)
| LetInE (Valbind (id, rhs), body) ->
    let rhsty  = typ (lvl+1) env rhs in
    let letenv = (id, generalize lvl rhsty) :: env in
    typ lvl letenv body
| FixE (fname, Abs (id, body)) ->
    let ptyv       = VarTy (new_typevar lvl) in
    let rtyv       = VarTy (new_typevar lvl) in
    let f_body_env =
      (id, TypeScheme ([], ptyv))
        :: (fname, TypeScheme ([], FunTy (ptyv, rtyv)))
        :: env in
    let rtyp = typ lvl f_body_env body in
    unify lvl rtyp rtyv;
    FunTy (ptyv, rtyv)
| CondE [] -> failwith "CondE with empty cond list"
| CondE ((guard, body) :: rest) ->
    let rec iter body_ty = function
    | [] -> body_ty
    | (guard, body) :: rest ->
        let guard_ty = typ lvl env guard in
        unify lvl guard_ty BoolTy;
        let body_ty' = typ lvl env body in
        unify lvl body_ty body_ty';
        iter body_ty rest
    in
    let guard_ty = typ lvl env guard in
    unify lvl guard_ty BoolTy;
    let body_ty = typ lvl env body in
    iter body_ty rest

(* records --------------------------------------- *)
| RecE fields ->
    let rec typ_of_fields lvl env = function
    | [] -> []
    | (id, exp) :: rest ->
        (id, typ lvl env exp) :: typ_of_fields lvl env rest
    in
    RecTy (typ_of_fields lvl env fields, None)

| SelectE (exp, id) ->
    let expty = typ lvl env exp in
    let fieldty = VarTy (new_typevar lvl) in
    unify lvl expty (RecTy ([(id, fieldty)], Some (new_typevar lvl)));
    fieldty

| RecUpdE (exp, id, newexp) as e -> raise (NotImplemented e)

(* staged computations *)
| ValueE _  | BoxE _  | UnboxE _  | RunE _  | LiftE _ as e ->
    raise (NotImplemented e)

let stdenv =
  let arith_op_ty = TypeScheme ([], FunTy (IntTy, FunTy (IntTy, IntTy))) in
  let ref0 id = ref (NoLink id, 0) in

  List.map (fun id -> (id, arith_op_ty)) [ "+"; "-"; "*"; "/" ]
    @ [ ("=", TypeScheme ( [ ref0 "a" ]
                         , FunTy (VarTy (ref0 "a"), VarTy (ref0 "a")) ))
      ; ("::", TypeScheme ( [ ref0 "a"]
                          , FunTy ( VarTy (ref0 "a")
                                  , FunTy ( ListTy (VarTy (ref0 "a"))
                                          , ListTy (VarTy (ref0 "a"))))))
      ; ("head", TypeScheme ( [ ref0 "a" ]
                            , FunTy ( ListTy (VarTy (ref0 "a"))
                                    , VarTy (ref0 "a"))))
      ; ("tail", TypeScheme ( [ ref0 "a" ]
                            , FunTy ( ListTy (VarTy (ref0 "a"))
                                    , VarTy (ref0 "a"))))
      ; ("empty", TypeScheme ( [ ref0 "a" ]
                             , FunTy (ListTy (VarTy (ref0 "a")), BoolTy)))
      ; ("nth", TypeScheme ( [ ref0 "a" ]
                           , FunTy (IntTy, ListTy (VarTy (ref0 "a")))))
      ]
