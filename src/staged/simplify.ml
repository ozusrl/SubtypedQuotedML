open Common
open Types

module IdMap = Map.Make(struct
  let compare = Pervasives.compare
  type t = string
end)

let simplify_type (ty : ty) : ty =
  let (Scheme (vars, _)) = generalize (-1) (FieldType ty) in

  let rec find_rhos_tyrec set tyrec = match norm_tyrec tyrec with
  | EmptyRec -> set
  | Rho recvar ->
      begin match !recvar with
      | NoLink id, _, _ ->
          if IdSet.mem id set then
            IdSet.remove id set
          else
            IdSet.add id set
      | LinkTo _, _, _ -> failwith "unexpected case in find_rhos_tyrec: should be a bug in norm_tyrec"
      end
  | Row (_, _, rest) -> find_rhos_tyrec set rest
  in

  (* find_rhos: search in type and collect rho variables to be simplified *)
  let rec find_rhos set = function
  | TInt | TBool | TUnit -> set
  | TPair (t1, t2) | TFun (t1, t2) ->
      let set' = find_rhos set t1 in
      find_rhos set' t2
  | TList ty | TRef ty -> find_rhos set ty
  | TRec tyrec -> find_rhos_tyrec set tyrec
  | TBox (tyrec, ty) ->
      let set' = find_rhos_tyrec set tyrec in
      find_rhos set' ty
  | TVar typevar ->
      begin match !typevar with
      | NoLink _, _ -> set
      | LinkTo ty, _ -> find_rhos set ty
      end
  in

  let find_rhos = find_rhos IdSet.empty in

  let rec rho_id_of_tyrec = function
  | EmptyRec -> None
  | Rho recvar ->
      begin match !recvar with
      | NoLink id, _, _ -> Some id
      | LinkTo _, _, _ -> failwith "unexpected case in rho_id_of_tyrec: should be a bug in norm_tyrec"
      end
  | Row (_, _, rest) -> rho_id_of_tyrec rest
  in

  let rec elim_fts_tyrec btms rhos tyrec =

    let rec iter elim = function
    | EmptyRec -> EmptyRec
    | Rho recvar ->
        if elim then
          let (link, lvl, idset) = !recvar in
          Rho (ref (link, lvl, IdSet.diff idset btms))
        else
          Rho recvar
    | Row (id, FieldVar fv, rest) ->
        if elim then
          match !fv with
          | NoLink type_id, lvl ->
              if List.mem (FV fv) vars then
                elim_fts_tyrec (IdSet.add id btms) rhos rest
              else
                Row (id, FieldVar fv, elim_fts_tyrec btms rhos rest)
          | LinkTo _, _ -> failwith "unexpected case in elim_fts_tyrec: should be a bug in norm_tyrec"
        else
          Row (id, FieldVar fv, elim_fts_tyrec btms rhos rest)
    | Row (id, FieldType ty, rest) -> Row (id, FieldType (elim_fts rhos ty), rest)
    | Row (id, f, rest) -> Row (id, f, iter elim rest)
    in

    let tyrec' = norm_tyrec tyrec in

    match rho_id_of_tyrec tyrec' with
    | None -> tyrec'
    | Some rho -> iter (IdSet.mem rho rhos) tyrec'

  and elim_fts (rhos : IdSet.t) : ty -> ty = function
  | TInt | TBool | TUnit as ty -> ty
  | TPair (t1, t2) -> TPair (elim_fts rhos t1, elim_fts rhos t2)
  | TList ty -> TList (elim_fts rhos ty)
  | TRef ty -> TList (elim_fts rhos ty)
  | TFun (t1, t2) -> TFun (elim_fts rhos t1, elim_fts rhos t2)
  | TVar typevar ->
      begin match !typevar with
      | NoLink _, _ -> TVar typevar
      | LinkTo typevar, _ -> elim_fts rhos typevar
      end
  | TBox (tyrec, ty) -> TBox (elim_fts_tyrec IdSet.empty rhos tyrec, elim_fts rhos ty)
  | TRec tyrec -> TRec (elim_fts_tyrec IdSet.empty rhos tyrec)
  in

  elim_fts (find_rhos ty) ty

module IdGen' = IdGen.IdGen (struct end)
open IdGen'

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
  | TVar typevar ->
      begin match !typevar with
      | NoLink id, lvl -> TVar (ref (NoLink (get_name id), lvl))
      | LinkTo ty, lvl -> TVar (ref (LinkTo (gen_names ty), lvl))
      end
  | TBox (tyrec, ty) -> TBox (gen_names_tyrec tyrec, gen_names ty)
  in

  gen_names ty
