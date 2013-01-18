open Common
open Env

type ty =
  | BottomTy
  | ConstTy  of id
  | ArrowTy  of ty * ty
  
  | TyVar    of int
  | BoxTy    of record * ty
  | TyScmEnv of record * record
  | FieldVar of int

  | TyScheme of type_var list * ty
  | RecordTy of record

and type_var =
  | TV of int (* type variable ? *)
  | EV of int (* type environment variable ? *)
  | FV of int (* field variable ? *)

and record =
  | CRec of concrete_record
  | RRec of row_record

and concrete_record = (string * ty) list
and row_record      = concrete_record * int

let int_ty  = ConstTy "int"
let bool_ty = ConstTy "bool"

let last_tyvar = ref 0
let fresh_var _ =
  let tyvar = !last_tyvar in
  last_tyvar := tyvar + 1; tyvar
let reset_last_tyvar _ = last_tyvar := 0

(* return substitution function from substitution list (list of type var,
 * substitution pairs). only variables (TyVar, FieldVar, RRec's row variable)
 * can be subtstituted *)
let rec getsub = function
| [] -> (function
  | TV n -> TyVar n
  | FV n -> FieldVar n
  | EV n -> RecordTy (RRec ([], n)))
| (TyVar i, ty)    :: cs -> (fun v -> if v = TV i then ty else getsub cs v)
| (FieldVar i, ty) :: cs -> (fun v -> if v = FV i then ty else getsub cs v)
| (RecordTy (RRec ([], i)), p) :: cs ->
    (fun v -> if v = EV (i) then p else getsub cs v)
| _ -> failwith "is not in substitution form lol."

(* substitution applications *{{{***********************************)
let rec apply_sub_ty sub = function
| BottomTy -> BottomTy
| ConstTy t -> ConstTy t
| ArrowTy (t1, t2) -> ArrowTy (apply_sub_ty sub t1, apply_sub_ty sub t2)
| TyVar i -> sub (TV i)
| BoxTy (env, ty) -> BoxTy (apply_sub_env sub env, apply_sub_ty sub ty)
| TyScmEnv (env1, env2) -> TyScmEnv (apply_sub_env sub env1, apply_sub_env sub env2)
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

and apply_sub_lst sub lst =
  List.map (fun (var, ty) -> (var, apply_sub_ty sub ty)) lst

and apply_sub_env sub = function
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

and apply_sub_env_lst sub = List.map (apply_sub_env sub)
(* substitution applications *}}}************************************)


(* extension of type environments {{{ *******************************)

(* add variable with type ty to list of (variable, type) pairs *)
and add_to_list var ty = function
| [] -> [(var, ty)]
| (var', ty') :: rest when var = var' -> (var, ty) :: rest
| (var', ty') :: rest -> (var', ty') :: (add_to_list var ty rest)

(* add variable with type ty to record *)
and add_rec var ty env = 
  match env with
  | CRec ce -> CRec (add_to_list var ty ce)
  | RRec (ce, i) -> RRec (add_to_list var ty ce, i)

(* add variable with type ty to list of records (type scheme envs)
 * return (subt, new record) pair. subst value is required when type is added to
 * a record with row variable (because later we need to unify that row variable
 * with substitution generated here to get required type information) *)
and add var ty = function
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


(* instantiate type scheme *)
and instantiate env =
  let inst (tt:ty) = match tt with
  | TyScheme (vars, ty) ->
      (* get substitution of quantified type variable as a pair *)
      let mk_sub = function
      | TV i -> (TyVar i, TyVar (fresh_var ()))
      | FV i -> (FieldVar i, FieldVar (fresh_var ()))
      | EV i -> (RecordTy (RRec ([], i)), RecordTy (RRec ([], fresh_var ())))
      in
      (* .. make substitution functions for each type variable *)
      let substs = List.map mk_sub vars in
      (* .. and apply it to type scheme to get env \w instantiated types *)
      apply_sub_ty (getsub substs) ty
  | _ -> tt
  in
  match env with
  | CRec ce -> CRec (List.map (fun (x, tt) -> (x, inst tt)) ce)
  | RRec (ce, n) -> RRec (List.map (fun (x, tt) -> (x, inst tt)) ce, n)
