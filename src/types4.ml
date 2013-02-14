open Common
open Sexplib.Sexp
open Sexplib.Conv

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
with sexp

type linkvar =
  | TV of typevar
  | FV of fieldvar
  | RV of recvar
  with sexp

type tyscm = TypeScheme of linkvar list * ty with sexp

type tenv = (id * tyscm) list with sexp

let show_type ty = to_string (sexp_of_ty ty)

(* ---}}}---------------------------------------------------------------------*)

(* link operations ---{{{-----------------------------------------------------*)

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

let new_recvar (lvl : int) (ids : id list) : recvar =
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
  let ss = List.map (fun tv -> tv, new_name ()) tvs in

  let rec subst_field field ss = match field, ss with
  | _, [] -> field
  | FieldType ty, ss -> FieldType (subst ty ss)
  | Bot, _ -> Bot
  | FieldVar fieldvar, ((l, t) :: rest) ->
      if FV fieldvar = l then FieldVar (ref (NoLink t, lvl))
      else subst_field field rest

  and subst_tyrec tyrec ss = match tyrec, ss with
  | _, [] -> tyrec
  | EmptyRec, _ -> EmptyRec
  | Rho recvar, ((l, t) :: rest) ->
      let (_, _, ids) = !recvar in
      if RV recvar = l then Rho (ref (NoLink t, lvl, ids))
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
  | TVar typevar, (l, t) :: rest ->
      if l = TV typevar then TVar (ref (NoLink t, lvl)) else subst ty rest
  in
  subst ty ss

(* ---}}}---------------------------------------------------------------------*)

(* norm ty ---{{{-------------------------------------------------------------*)

let rec norm_ty = function
| TVar typevar -> (match !typevar with
  | LinkTo t1, _ ->
      let t2 = norm_ty t1 in
      set_link typevar (LinkTo t2);
      t2
  | _ -> TVar typevar)
| t -> t

(* ---}}}---------------------------------------------------------------------*)

(* freetyvars ---{{{----------------------------------------------------------*)

let rec freetyvars (ty : ty) : linkvar list =
  let rec freetyvars_tyrec = function
  | EmptyRec -> []
  | Rho recvar -> (match !recvar with
    | (NoLink id, lvl, _) -> [RV recvar]
    | (LinkTo l, _, _) -> freetyvars_tyrec l)
  | Row (_, _, tyrec) -> freetyvars_tyrec tyrec
  in
  let rec freetyvars_typevar v = match !v with
  | (NoLink _, _) -> [TV v]
  | (LinkTo l, lvl) -> freetyvars l
  in
  match norm_ty ty with
  | TInt  -> []
  | TBool -> []
  | TList t
  | TRef  t -> freetyvars t
  | TFun (t1, t2) -> freetyvars t1 @ freetyvars t2
  | TRec tyrec    -> freetyvars_tyrec tyrec
  | TVar typevar  -> freetyvars_typevar typevar

(* ---}}}---------------------------------------------------------------------*)

(* unification --{{{----------------------------------------------------------*)

let link_typevar_to_ty (typevar : typevar) (ty : ty) : unit =
  (*Printf.printf "link_typevar_to_ty: %s -> %s\n" (show_type (TVar typevar)) (show_type ty);*)
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
    (*;Printf.printf "link_typevar_to_ty (after): %s -> %s\n\n" (show_type (TVar typevar)) (show_type ty)*)

let rec unify (lvl : int) (t1 : ty) (t2 : ty) : unit =
  (*Printf.printf "unify %s with %s.\n" (show_type t1) (show_type t2);*)
  let t1' = norm_ty t1 in
  let t2' = norm_ty t2 in
  (*Printf.printf "types after norm_ty %s --- %s\n" (show_type t1') (show_type t2');*)
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
      link_typevar_to_ty typevar ty;

  | TInt,  TInt
  | TBool, TBool -> ()
  | TList ty1, TList ty2 -> unify lvl ty1 ty2
  | TRef ty1,  TRef ty2  -> unify lvl ty1 ty2
  | TFun (a, b), TFun (c, d) -> unify lvl a c; unify lvl b d
  (*| TRec tyrec1, TRec tyrec2 -> () [> TODO <]*)

  | _, _ -> failwith "can't unify lol"

(* ---}}}---------------------------------------------------------------------*)

(* type inference --{{{-------------------------------------------------------*)

let rec generalize lvl (t : ty) : tyscm =
  let notfreeincontext link =
    link_lvl link >= lvl
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
    let rho = Rho (new_recvar lvl [id]) in
    let retty = TVar (new_typevar lvl) in

    let expty = typ lvl env exp in
    unify lvl expty (TRec (Row (id, FieldType retty, rho)));
    retty

| RecUpdE (exp, id, extexp) ->
    let rho = Rho (new_recvar lvl [id]) in
    let record = TRec (Row (id, FieldVar (new_fieldvar lvl), rho)) in

    let expty  = typ lvl env exp in
    unify lvl record expty;

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
