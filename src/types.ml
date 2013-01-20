open Common
open Sexplib.Sexp
open Sexplib.Conv

type ty =
  | BottomTy
  | ConstTy  of id
  | ArrowTy  of ty * ty
  | BoxTy    of record * ty

  | FieldVar of int
  | TyVar    of int

  | TyScheme of qtype_val list * ty
  | RecordTy of record

(* universally quantified type variables *)
and qtype_val =
  | TV of int (* type variable *)
  | EV of int (* type environment variable (used for rows) TODO: bundan emin ol *)
  | FV of int (* field variable *)

and record =
  | CRec of concrete_record
  | RRec of row_record

and concrete_record = (string * ty) list
and row_record      = concrete_record * int
and subst           = qtype_val -> ty
with sexp

let show_type ty = to_string (sexp_of_ty ty)

let int_ty  = ConstTy "int"
let bool_ty = ConstTy "bool"

let last_tyvar = ref 0
let fresh_var _ =
  let tyvar = !last_tyvar in
  last_tyvar := tyvar + 1; tyvar
let reset_last_tyvar _ = last_tyvar := 0
let fresh_tyvar _ = TyVar (fresh_var ())

let (test_env:record list) =
  [ CRec [ ("+", ArrowTy (int_ty, ArrowTy (int_ty, int_ty)))
         ; let argvar = fresh_var () in
           ("=", TyScheme ([TV argvar], (ArrowTy (TyVar argvar, ArrowTy (TyVar argvar, bool_ty)))))
         ; let argvar = fresh_var () in
           ("id", TyScheme ([TV argvar], (ArrowTy (TyVar argvar, TyVar argvar))))
         ]
  ]

(* return substitution function from substitution list (list of type var,
 * substitution pairs). only variables (TyVar, FieldVar, RRec's row variable)
 * can be subtstituted *)
let rec getsub : (ty * ty) list -> (qtype_val -> ty) = function
| [] -> (function
  | TV n -> TyVar n
  | FV n -> FieldVar n
  | EV n -> RecordTy (RRec ([], n)))
| (TyVar i, ty)    :: cs -> (fun v -> if v = TV i then ty else getsub cs v)
| (FieldVar i, ty) :: cs -> (fun v -> if v = FV i then ty else getsub cs v)
| (RecordTy (RRec ([], i)), p) :: cs ->
    (fun v -> if v = EV (i) then p else getsub cs v)
| ((ty1, ty2) :: _) -> failwith ("getsub: " ^ show_type ty1 ^ " --- " ^ show_type ty2 ^ " is not in substitution form lol.")
(*| ((ty1, ty2) :: _) -> failwith "getsub"*)

(* substitution applications *{{{***********************************)
let rec apply_sub_ty (sub : subst) : ty -> ty  = function
| BottomTy -> BottomTy
| ConstTy t -> ConstTy t
| ArrowTy (t1, t2) -> ArrowTy (apply_sub_ty sub t1, apply_sub_ty sub t2)
| TyVar i -> sub (TV i)
| BoxTy (env, ty) -> BoxTy (apply_sub_env sub env, apply_sub_ty sub ty)
| FieldVar i -> sub (FV i)
| TyScheme (vars, ty) ->
    let mask_subst =
      let make_new_sub newsub var = 
        fun v -> if v = var then getsub [] v else newsub v
      in
      List.fold_left make_new_sub
    in
    TyScheme (vars, apply_sub_ty (mask_subst sub vars) ty)
| RecordTy env -> RecordTy (apply_sub_env sub env)

and apply_sub_lst (sub : subst) (lst : (id * ty) list) =
  List.map (fun (var, ty) -> (var, apply_sub_ty sub ty)) lst

and apply_sub_env (sub : subst) : record -> record = function
| CRec ce -> CRec (apply_sub_lst sub ce)
| RRec (ce, n) -> (match sub (EV n) with
  | RecordTy (CRec ce') ->
      List.fold_left (fun y (x, t) -> add_rec x t y)
        (CRec ce') (apply_sub_lst sub ce)
  | RecordTy (RRec (ce', v)) ->
      let CRec newce = List.fold_left (fun y (x, t) -> add_rec x t y) (CRec ce')
                          (apply_sub_lst sub ce)
      in RRec (newce, v)
  | _ -> failwith "no sub lol")

and apply_sub_env_lst (sub : subst) : record list -> record list =
  List.map (apply_sub_env sub)

and apply_sub_const (sub : subst) ((ty1, ty2) : (ty * ty)) =
  (apply_sub_ty sub ty1, apply_sub_ty sub ty2)

and apply_sub_const_lst (sub : subst) : (ty * ty) list -> (ty * ty) list =
  List.map (apply_sub_const sub)

and compose (sub1 : subst) (sub2 : subst) : subst =
  fun v -> apply_sub_ty sub2 (sub1 v)

(* substitution applications *}}}************************************)


(* extension of type environments {{{ *******************************)

(* add variable with type ty to list of (variable, type) pairs *)
and add_to_list (var : id) (ty : ty) : (id * ty) list -> (id * ty) list = function
| [] -> [(var, ty)]
| (var', ty') :: rest when var = var' -> (var, ty) :: rest
| (var', ty') :: rest -> (var', ty') :: (add_to_list var ty rest)

(* add variable with type ty to record *)
and add_rec (var : id) (ty : ty) : record -> record = function
| CRec ce -> CRec (add_to_list var ty ce)
| RRec (ce, i) -> RRec (add_to_list var ty ce, i)

(* add variable with type ty to list of records (type scheme envs)
 * return (subt, new record) pair. subst value is required when type is added to
 * a record with row variable (because later we need to unify that row variable
 * with substitution generated here to get required type information) *)
and add (var : id) (ty : ty) : record list -> (subst * record list) = function
| env :: rest -> (match env with
  | CRec (ce) ->
      (* type environment doesn't have row variable,
       * search and replace is not necessary since just adding to head
       * position has the same effect FIXME bundan emin ol *)
      (getsub [], (add_rec var ty env) :: rest)
  | RRec (ce, row) ->
      if List.exists (fun (var', _) -> var = var') ce then
        (* concrete environment already has the var, just add new value *)
        (getsub [], (add_rec var ty env) :: rest)
      else
        (* concrete environment doesn't have the `var`, we need to add a
         * constraint to make sure row variable will be unified with a type
         * environment that has the `var` with correct type(`ty`) *)
        let new_row_var = fresh_var () in
        let new_row_sub = getsub
          [ ( RecordTy (RRec ([], row))
            , RecordTy (RRec ([(var, FieldVar (fresh_var ()))], new_row_var))) ]
        in
        (* TODO: bu kismi anla ve dogru calistigindan emin ol *)
        ( new_row_sub
        , RRec (apply_sub_lst new_row_sub (add_to_list var ty ce), new_row_var)
            :: (apply_sub_env_lst new_row_sub rest)))
  | [] -> failwith "env list cannot be empty lol"

(* extension of type environments }}} *******************************)


(* normalization of row variables {{{ ---------------------------------------*)


and esort (fields : (string * ty) list) : (string * ty) list =
  List.stable_sort (fun (x, a) (y, b) -> compare x y) fields

(* return a substitutions  `(ty * ty) list` from concrete record field lists
 * `(string * ty) list` *)

(*TODO: why adding bottom types ? *)
and unify_concrete (fields1 : (id * ty) list) (fields2 : (id * ty) list)
    : (ty * ty) list =
  let rec aux slist l1 l2 = match l1, l2 with
  | [], [] -> slist
  | [], (_, ty) :: l2' -> aux ((ty, BottomTy) :: slist) l1 l2'
  | (_, ty) :: l1', [] -> aux ((ty, BottomTy) :: slist) l1' l2
  | (var1, ty1) :: l1', (var2, ty2) :: l2' -> (match compare var1 var2 with
    | -1 -> aux ((ty1, BottomTy) :: slist)  l1' l2
    | 0  -> aux ((ty1, ty2) :: slist)       l1' l2'
    | 1  -> aux ((ty2, BottomTy) :: slist)  l1 l2'
    | _ -> failwith"")
  in
  aux [] (esort fields1) (esort fields2)

and unify_asymmetric (fields1 : (id * ty) list) (fields2 : (id * ty) list)
    : ((ty * ty) list * (id * ty) list) =
  let rec aux slist row l1 l2 = match l1, l2 with
  | [], [] -> (slist, row)
  | [], (var, ty) :: l2' -> aux slist ((var, ty) :: row) l1 l2'
  | (var, ty) :: l1', [] -> aux slist ((var, BottomTy) :: row) l1' l2
  | (var1, ty1) :: l1', (var2, ty2) :: l2' -> (match compare var1 var2 with
    | -1 -> aux ((ty1, BottomTy) :: slist) row l1' l2
    | 0  -> aux ((ty1, ty2) :: slist) row l1' l2'
    | 1  -> aux slist ((var2, ty2) :: row) l1 l2'
    | _ -> failwith"")
  in aux [] [] (esort fields1) (esort fields2)

and unify_symmetric (fields1 : (id * ty) list) (fields2 : (id * ty) list) 
    : ((ty * ty) list * (id * ty) list * (id * ty) list) =
  let rec aux slist row1 row2 l1 l2 = match l1, l2 with
  | [], [] -> (slist, row1, row2)
  | [], (var, ty) :: l2' ->
      aux slist ((var, ty) :: row1) ((var, BottomTy) :: row2) l1 l2'
  | (var, ty) :: l1', [] ->
      aux slist ((var, BottomTy) :: row1) ((var, ty) :: row2) l1' l2
  | (var1, ty1) :: l1', (var2, ty2) :: l2' -> (match compare var1 var2 with
    | -1 -> aux slist ((var1, BottomTy) :: row1) ((var1, ty1) :: row2) l1' l2
    | 0  -> aux ((ty1, ty2) :: slist)
              ((var1, BottomTy) :: row1)
              ((var2, BottomTy) :: row2)
              l1' l2'
    | 1  -> aux slist ((var2, ty2) :: row1) ((var2, BottomTy) :: row1) l1 l2'
    | _ -> failwith"")
  in
  aux [] [] [] (esort fields1) (esort fields2)

and unify_symmetric_one_row (fields1 : (id * ty) list) (fields2 : (id * ty) list)
    : (ty * ty) list = 
  let rec aux slist l1 l2 = match l1, l2 with
  | [], [] -> slist
  | (var1, ty1) :: l1', (var2, ty2) :: l2' when var1 = var2 ->
      aux ((ty1, ty2) :: slist) l1' l2'
  | _ -> failwith "failed in unify_symmetric_one_row"
  in 
  aux [] (esort fields1) (esort fields2)


(* normalization of row variables }}} ---------------------------------------*)

and normalize : (ty * ty) list -> (ty * ty) list = function
| [] -> []
| (ty1, ty2) :: rest when ty1 = ty2 -> normalize rest
| (ty1, ty2) :: rest -> (match ty1, ty2 with
  | FieldVar i, ty
  | ty, FieldVar i -> eliminate (FV i) ty rest

  | BottomTy, ty
  | ty, BottomTy -> failwith "conflict in constraint"

  | TyVar i, ty
  | ty, TyVar i -> eliminate (TV i) ty rest

  | ArrowTy (l1, r1), ArrowTy (l2, r2) ->
      normalize ((l1, l2) :: (r1, r2) :: rest)

  | BoxTy (env1, box1), BoxTy (env2, box2) ->
      normalize ((box1, box2) :: (RecordTy env1, RecordTy env2) :: rest)

  | RecordTy env1, RecordTy env2 -> (match env1, env2 with
    | RRec ([], i), _ -> eliminate (EV i) ty2 rest
    | _, RRec ([], i) -> eliminate (EV i) ty1 rest
    | RRec (fields1, i1), RRec (fields2, i2) ->
        if i1 = i2 then
          let cl = unify_symmetric_one_row fields1 fields2 in
          normalize (cl @ rest)
        else
          let (cl, row1, row2) = unify_symmetric fields1 fields2 in
          let new_row = fresh_var () in
          normalize ((RecordTy (RRec ([], i1)), RecordTy (RRec (row1, new_row)))
            :: (RecordTy (RRec ([], i2)), RecordTy (RRec (row2, new_row)))
            :: (cl @ rest))
    | RRec (env1, i), CRec (env2) ->
        let cl, row = unify_asymmetric env1 env2 in
        normalize ((RecordTy (RRec ([], i)), RecordTy (CRec row)) :: (cl @
        rest))
    | CRec (env1), CRec (env2) -> normalize ((unify_concrete env1 env2) @ rest)
    | CRec (env1), RRec (env2, i) -> normalize ((ty2, ty1) :: rest))
  | _ -> failwith "normalize fail")

and eliminate (var : qtype_val) (ty : ty) (substs : (ty * ty) list)
    : (ty * ty) list =
  let substs' =
    apply_sub_const_lst (fun m -> if m = var then ty else getsub [] m) substs 
  in
  let scl = normalize substs' in
  ((getsub [] var, apply_sub_ty (getsub scl) ty) :: scl)

(* instantiate type scheme *)
and instantiate (scm : record) : record =
  let inst (tt:ty) = match tt with
  | TyScheme (vars, ty) ->
      (* get substitution of quantified type variable as a pair *)
      let mk_sub = function
      | TV i -> (TyVar i, fresh_tyvar ())
      | FV i -> (FieldVar i, FieldVar (fresh_var ()))
      | EV i -> (RecordTy (RRec ([], i)), RecordTy (RRec ([], fresh_var ())))
      in
      (* .. make substitution functions for each type variable *)
      let substs = List.map mk_sub vars in
      (* .. and apply it to type scheme to get scm \w instantiated types *)
      apply_sub_ty (getsub substs) ty
  | _ -> tt
  in
  match scm with
  | CRec ce -> CRec (List.map (fun (x, tt) -> (x, inst tt)) ce)
  | RRec (ce, n) -> RRec (List.map (fun (x, tt) -> (x, inst tt)) ce, n)


let unify (eqs : (ty * ty) list) : qtype_val -> ty =
  getsub (normalize eqs)

let rec generalize (ty : ty) (elst : record list) : ty =
  let rec contains (lst : qtype_val list) (x : qtype_val) : bool =
    List.exists (fun a -> a = x) lst in

  let rec collapse : qtype_val list -> qtype_val list = function
  | [] -> []
  | v :: vs -> if contains vs v then collapse vs else v :: collapse vs
  in

  let rec remove (xs : qtype_val list)
      : qtype_val list -> qtype_val list = function
  | [] -> []
  | y :: ys -> if contains xs y then remove xs ys else y :: (remove xs ys)
  in

  let rec names (ty : ty) (acc : qtype_val list)
    : qtype_val list = match ty with
  | BottomTy -> acc
  | ConstTy _ -> acc
  | ArrowTy (a, b) -> names a (names b acc)
  | BoxTy (env, ty) -> env_names env (names ty acc)
  | FieldVar i -> if contains acc (FV i) then acc else (FV i) :: acc
  | TyVar i -> if contains acc (TV i) then acc else (TV i) :: acc
  | TyScheme (xs, ty) -> (remove xs (names ty [])) @ acc
  | RecordTy env -> env_names env acc
  and env_names (env : record) (acc : qtype_val list)
      : qtype_val list = match env with
  | CRec ce -> List.fold_left (fun y (x, t) -> names t y) acc ce
  | RRec (ce, n) -> List.fold_left (fun y (x, t) -> names t y)
                      (if contains acc (EV n) then acc else (EV n) :: acc) ce
  in

  let fvs : qtype_val list = names ty [] in
  let fv_envs : qtype_val list = List.fold_left (fun ys y -> (env_names y []) @ ys) [] elst in
  let bound_vars : qtype_val list = remove fv_envs fvs in
  TyScheme (collapse bound_vars, ty)

let rec infer (envs : record list) (exp : exp) (tyvar : ty) : subst =
  match envs with
| [] -> failwith "infer: at least one environment needed."
| env :: rest -> (match exp with
  | ConstE (CInt _) -> unify [(tyvar, int_ty)]
  | ConstE (CBool _) -> unify [(tyvar, bool_ty)]

  | IdE id -> unify [( RecordTy (RRec ([id, tyvar], fresh_var ()))
                      , RecordTy (instantiate env))]

  (*| IdE id -> unify [( RecordTy (instantiate env)*)
                      (*, RecordTy (RRec ([id, tyvar], fresh_var ())))]*)

  | AppE (e1, e2) ->
      let new_tyvar = fresh_tyvar () in
      let s1 = infer envs e1 (ArrowTy (new_tyvar, tyvar)) in
      let s2 = infer (apply_sub_env_lst s1 envs) e2 (apply_sub_ty s1 new_tyvar) in
      compose s2 s1

  | AbsE (Abs (id, body)) -> 
      let domain = fresh_tyvar () and range = fresh_tyvar () in
      let (sub0, envs') = add id domain envs in
      let sub1 = infer envs' body (apply_sub_ty sub0 range) in
      let sub2 = unify [( apply_sub_ty sub1 tyvar
                        , apply_sub_ty sub1 (ArrowTy (domain, range)))]
      in
      compose sub2 (compose sub1 sub0)

  | BoxE e ->
      let fresh1 = fresh_tyvar () in
      let newenv = RRec ([], fresh_var ()) in
      let sub1 = infer (newenv :: envs) exp fresh1 in
      let sub2 = unify [( apply_sub_ty sub1 tyvar
                         , apply_sub_ty sub1 (BoxTy (newenv, fresh1)))]
      in
      compose sub2 sub1

  | UnboxE e ->
      let newenv = RRec ([], fresh_var ()) in
      let sub1 = infer rest exp (BoxTy (newenv, tyvar)) in
      let sub2 = unify [( RecordTy (apply_sub_env sub1 newenv)
                         , RecordTy (apply_sub_env sub1 (instantiate env)))]
      in
      compose sub2 sub1

)

