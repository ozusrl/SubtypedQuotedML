open Common
open Format
open Types4

let infix_ops = ["+"; "-"; "*"; "/"; "="; "::"]

let rec print_exp = function
| IdE id -> print_string id
| ConstE const -> print_const const
| EmpLstE -> print_string "[]"
| AppE (e1, e2) -> (match e1, e2 with
  | AppE (IdE i, e1'), e2' when List.mem i infix_ops ->
      printf "@[<hov 2>(";
      print_exp e1';
      printf "@;<1 2>%s@;<1 2>" i;
      print_exp e2';
      printf ")@]"
  | _ ->
      printf "@[<hov 2>(";
      print_exp e1;
      printf "@;<1 2>";
      print_exp e2;
      printf ")@]")
| AbsE abs -> print_abs abs
| LetInE (dec, exp) ->
    printf "@[<hv 2>let@ ";
    print_dec dec;
    printf "@;<1 -2>in@;<1 -2>";
    print_exp exp;
    printf "@]";
| FixE (id, abs) ->
    printf "@[<hov 2>fix@ %s@ ->@ " id; print_abs abs; printf "@]";
| CondE conds ->
    let print_cond s (g, b) =
      printf "@[<hov 2>%s@ " s;
      print_exp g;
      printf "@ then@ ";
      print_exp b;
      printf "@]";
    in
    let first_cond = List.hd conds in
    let rest_conds = List.tl conds in
    let rec print_conds = function
    | [] -> ()
    | [c] -> print_cond "else" c
    | f :: r ->
        print_cond "elseif" f;
        print_space ();
        print_conds r
    in
    open_hvbox 0;
    print_cond "if" first_cond;
    print_space ();
    print_conds rest_conds;
    close_box ()
| RefE exp   -> printf "@[<hov 2>ref@ "; print_exp exp; printf "@]";
| DerefE exp -> print_string "!"; print_exp exp
| AssignE (e1, e2) ->
    printf "@[<hov 2>";
    print_exp e1; printf "@ :=@ "; print_exp e2;
    printf "@]";
| ValueE v   -> print_value v
| BoxE exp   -> printf "@[<hov 2><"; print_exp exp; printf ">@]"
| UnboxE exp -> printf "@[<hov 2>~("; print_exp exp; printf")@]"
| RunE exp   -> printf "@[<hov 2>run("; print_exp exp; printf ")@]"
| LiftE exp  -> printf "lift(@;<0 2>"; print_exp exp; printf ")"
| EmptyRecE  -> print_string "{}"
| SelectE (exp, field) -> print_exp exp; printf ".%s" field
| RecUpdE (r, id, exp) ->
    printf "@[<hov 2>{{@ ";
    print_exp r;
    printf ".%s@ :=@ " id;
    print_exp exp;
    printf "@ }}@]"

and print_const = function
| CInt i  -> print_int i
| CBool b -> print_bool b

and print_abs = function
| Abs (id, body) ->
    printf "@[<hov 2>(fun@ %s@ ->@ " id; print_exp body; printf ")@]"

and print_dec = function
| Valbind (id, exp) -> printf "@[<hov 2>%s@ =@ " id; print_exp exp; printf "@]"

and print_value = function
| ConstV c -> print_const c
| ClosV f  -> print_funval f
| BoxV e   -> print_exp (BoxE e)
| ListV vals ->
    let rec iter = function
    | []      -> ()
    | [v]     -> print_value v;
    | v :: vs -> print_value v; printf "@;<1 0>; "; iter vs
    in
    printf "@[<hov 0>[ ";
    iter vals;
    printf "@ ]@]"
| RecV fields ->
    let rec print_fields = function
    | []      -> ()
    | [(k,v)] -> printf "@[<hov 2>%s@ =@ " k; print_value v; printf "@]"
    | (k, v) :: rest ->
        printf "@[<hov 2>%s@ =@ " k; print_value v; printf ",@ @]";
        print_fields rest
    in
    printf "@[<hov 2>{@ "; print_fields fields; printf "@ }@]"
| RefV v -> printf "ref@ "; print_value !v
| UnitV -> print_string "()"

and print_funval = function
| StdFun (StdCurry (id, _))
| StdFun (StdFunction (id, _)) -> printf "<stdfun %s>" id
| Closure (env, id, body)      -> print_abs (Abs (id, body))

let rec print_ty = function
| TInt     -> print_string "int"
| TBool    -> print_string "bool"
| TList ty -> printf "["; print_ty ty; printf "]"
| TRef ty  -> printf "ref "; print_ty ty
| TFun (t1, t2) ->
    printf "@[<hov 2>(";
    print_ty t1; printf "@ ->@ "; print_ty t2;
    printf ")@]"
| TRec tyrec   -> print_tyrec tyrec
| TVar typevar -> print_typevar typevar

and print_tyrec = function
| EmptyRec   -> print_string "{}"
| Rho recvar -> print_recvar recvar
| Row (id, field, tyrec) ->
    printf "@[<hov 2>{@ %s@ =@ " id;
    print_field field;
    print_tyrec_tail tyrec;
    printf "@]"

and print_tyrec_tail = function
| EmptyRec -> print_string "}"
| Rho recvar -> print_recvar_tail recvar
| Row (id, field, tyrec) ->
    printf ",@ %s@ =@ " id;
    print_field field;
    print_space ();
    print_tyrec_tail tyrec

and print_field = function
| FieldType ty -> print_ty ty
| FieldVar fv  -> print_fieldvar fv
| Bot          -> print_string "_|_"

and print_typevar typevar = match !typevar with
| NoLink id, _ -> printf "'%s" id
| LinkTo ty, _ -> print_ty ty

and print_recvar recvar = match !recvar with
| NoLink id, _, btms -> printf "{'%s" id; print_idset btms; printf "@ }"
| LinkTo tyrec, _, _ -> print_tyrec tyrec

and print_recvar_tail recvar = match !recvar with
| NoLink id, _, btms -> printf "|@ '%s" id; print_idset btms; printf "@ }"
| LinkTo tyrec, _, _ -> print_tyrec_tail tyrec

and print_idset idset =
  let rec iter = function
  | []         -> ()
  | [id]       -> print_string id
  | id :: rest -> printf "%s,@ " id; iter rest
  in
  let elems = IdSet.elements idset in
  printf "@[<hov 2>[";
  iter elems;
  printf "]@]"

and print_fieldvar fieldvar = match !fieldvar with
| NoLink id, _    -> printf "'%s" id
| LinkTo field, _ -> print_field field
