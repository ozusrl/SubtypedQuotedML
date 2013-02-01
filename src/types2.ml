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

and typevar = (tyvarkind * int) ref

and tyvarkind =
  | NoLink of string  (* just a type variable *)
  | LinkTo of ty      (* equated to type ty *)
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
    in
    subst t ss

let set_tvkind tyvar newkind =
  let (kind, lvl) = !tyvar in
  tyvar := (newkind, lvl)

(*let rec norm_ty = function
| VarTy tyvar -> (match !tyvar with
  | LinkTo t1, _ ->
      let t2 = norm_ty t1 in
      set_tvkind tyvar (LinkTo t2);
      t2
  | _ -> VarTy tyvar)
| t -> t*)

let rec norm_ty = function
| IntTy -> IntTy
| BoolTy -> BoolTy
| FunTy (t1, t2) -> FunTy (norm_ty t1, norm_ty t2)
| VarTy tyvar -> (match !tyvar with
  | LinkTo t1, _ ->
      let t2 = norm_ty t1 in
      set_tvkind tyvar (LinkTo t2);
      t2
  | _ -> VarTy tyvar)
| ListTy t -> ListTy (norm_ty t)

let rec freetyvars t : typevar list = match norm_ty t with
| IntTy          -> []
| BoolTy         -> []
| FunTy (t1, t2) -> union (freetyvars t1) (freetyvars t2)
| VarTy tv       -> [tv]
| ListTy t1      -> freetyvars t1

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

let rec unify (t1 : ty) (t2 : ty) : unit =
  let t1' = norm_ty t1 in
  let t2' = norm_ty t2 in
  match (t1', t2') with
  | IntTy, IntTy -> ()
  | BoolTy, BoolTy -> ()
  | FunTy (t1', t2'), FunTy (t1'', t2'') ->
      unify t1' t1''; unify t2' t2''
  | VarTy tv1, VarTy tv2 ->
      if tv1 = tv2 then ()
      else
        let (_, tv1level) = !tv1 in
        let (_, tv2level) = !tv2 in
        if tv1level < tv2level then
          link_to_ty tv1 t2'
        else
          link_to_ty tv2 t1'
  | ListTy tv1, ListTy tv2 -> unify tv1 tv2
  | VarTy tv1, _ -> link_to_ty tv1 t2'
  | _, VarTy tv2 -> link_to_ty tv2 t1'
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
    unify funty (FunTy (argty, retty));
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
    unify rtyp rtyv;
    FunTy (ptyv, rtyv)
| CondE [] -> failwith "CondE with empty cond list"
| CondE ((guard, body) :: rest) ->
    let rec iter body_ty = function
    | [] -> body_ty
    | (guard, body) :: rest ->
        let guard_ty = typ lvl env guard in
        unify guard_ty BoolTy;
        let body_ty' = typ lvl env body in
        unify body_ty body_ty';
        iter body_ty rest
    in
    let guard_ty = typ lvl env guard in
    unify guard_ty BoolTy;
    let body_ty = typ lvl env body in
    iter body_ty rest

| e -> raise (NotImplemented e)

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
