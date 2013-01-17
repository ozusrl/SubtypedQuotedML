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

let int_ty = ConstTy "int"
let bool_ty = ConstTy "bool"

let last_tyvar = ref 0
let fresh_var _ =
  let tyvar = !last_tyvar in
  last_tyvar := tyvar + 1; tyvar
let reset_last_tyvar _ = last_tyvar := 0

(* return substitution function from substitution list
 * only variables ( TyVar, FieldVar, RRec's row variable ) can be subtstituted *)
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

(* apply substitution to type scheme *)
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
      List.fold_left (fun y (x, t) -> add x t y)
        (CRec ce') (apply_sub_lst sub ce)
  | RecordTy (RRec (ce', v)) ->
      let CRec newce = List.fold_left (fun y (x, t) -> add x t y) (CRec ce')
                          (apply_sub_lst sub ce)
      in RRec (newce, v)
  | _ -> failwith "no sub lol")

and apply_sub_env_lst sub = List.map (apply_sub_env sub)

and add var t = function
| CRec ce -> CRec (add_to_list var t ce)
| RRec (ce, i) -> RRec (add_to_list var t ce, i)

and add_to_list var t = function
| [] -> [(var, t)]
| (var', t') :: rest when var = var' -> (var, t) :: rest
| (var', t') :: rest -> (var', t') :: (add_to_list var t rest)

and instantiate env =
  let inst (tt:ty) = match tt with
  | TyScheme (vars, ty) ->
      let asdf = function
      | TV i -> (TyVar i, TyVar (fresh_var ()))
      | FV i -> (FieldVar i, FieldVar (fresh_var ()))
      | EV i -> (RecordTy (RRec ([], i)), RecordTy (RRec ([], fresh_var ())))
      in
      let bindings = List.map asdf vars in
      apply_sub_ty (getsub bindings) ty
  | _ -> tt
  in
  match env with
  | CRec ce -> CRec (List.map (fun (x, tt) -> (x, inst tt)) ce)
  | RRec (ce, n) -> RRec (List.map (fun (x, tt) -> (x, inst tt)) ce, n)
