open StagedCommon
open StagedTypes
open StagedIdGen

let simplify_type (ty : ty) : ty =
  let (Scheme (vars, _)) = generalize (-1) (FieldType ty) in

  let rec elim_fts_tyrec btms tyrec =
    match norm_tyrec tyrec with
    | EmptyRec -> failwith ""
    | Rho recvar ->
        (* recvar should be NoLink because of norm_tyrec call *)
        let (link, lvl, idset) = !recvar in
        Rho (ref (link, lvl, IdSet.diff idset btms))
    | Row (id, FieldVar fv, rest) ->
        (match !fv with
        | NoLink type_id, lvl ->
            if List.mem (FV fv) vars then
              elim_fts_tyrec (IdSet.add id btms) rest
            else
              Row (id, FieldVar fv, elim_fts_tyrec btms rest)
        | LinkTo _, _ -> failwith "unexpected case in elim_fts_tyrec: should be a bug in norm_tyrec")
    | Row (id, f, rest) -> Row (id, f, elim_fts_tyrec btms rest)
  in

  let elim_fts_tyrec = elim_fts_tyrec IdSet.empty in

  let rec elim_fts = function
  | TInt | TBool | TUnit as ty -> ty
  | TPair (t1, t2) -> TPair (elim_fts t1, elim_fts t2)
  | TList ty -> TList (elim_fts ty)
  | TRef ty -> TList (elim_fts ty)
  | TFun (t1, t2) -> TFun (elim_fts t1, elim_fts t2)
  | TVar typevar -> (match !typevar with
    | NoLink _, _ -> TVar typevar
    | LinkTo typevar, _ -> elim_fts typevar)
  | TBox (tyrec, ty) -> TBox (elim_fts_tyrec tyrec, elim_fts ty)
  | TRec tyrec -> TRec (elim_fts_tyrec tyrec)
  in

  elim_fts ty

module IdMap = Map.Make(struct
  let compare = Pervasives.compare
  type t = string
end)

module IdGen = StagedIdGen (struct end)
open IdGen

let rename_vars (ty : ty) : ty =
  let names = ref (IdMap.empty) in
  last_name := 0;

  let get_name id =
    try
      IdMap.find id !names
    with Not_found ->
      let name = new_name () in
      names := IdMap.add id name !names;
      name
  in

  let rec gen_names_field = function
  | FieldType ty -> FieldType ty
  | FieldVar fieldvar ->
      FieldVar (ref (match !fieldvar with
                     | NoLink id, lvl -> (NoLink (get_name id), lvl)
                     | LinkTo field, lvl -> (LinkTo (gen_names_field field), lvl)))
  | Bot -> Bot

  and gen_names_tyrec = function
  | EmptyRec -> EmptyRec
  | Rho recvar ->
      Rho (ref (match !recvar with
                | NoLink id, lvl, btms -> (NoLink (get_name id), lvl, btms)
                | LinkTo tyrec, lvl, btms -> (LinkTo (gen_names_tyrec tyrec), lvl, btms)))
  | Row (id, field, rest) -> Row (id, gen_names_field field, gen_names_tyrec rest)

  and gen_names = function
  | TInt | TBool | TUnit as ty -> ty
  | TPair (t1, t2) -> TPair (gen_names t1, gen_names t2)
  | TList t -> TList (gen_names t)
  | TRef t -> TRef (gen_names t)
  | TFun (t1, t2) -> TFun (gen_names t1, gen_names t2)
  | TRec tyrec -> TRec (gen_names_tyrec tyrec)
  | TVar typevar -> (match !typevar with
    | NoLink id, lvl -> TVar (ref (NoLink (get_name id), lvl))
    | LinkTo ty, lvl -> TVar (ref (LinkTo (gen_names ty), lvl)))
  | TBox (tyrec, ty) -> TBox (gen_names_tyrec tyrec, gen_names ty)
  in

  gen_names ty


