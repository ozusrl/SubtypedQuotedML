open Common
open Format
open Types2

let infix_ops = ["+"; "-"; "*"; "/"; "="; "::"]

let rec print_exp = function
| IdE id -> print_string id
| ConstE const -> print_const const
| EmpLstE -> print_string "[]"
| AppE (e1, e2) -> (match e1, e2 with
  | AppE (IdE i, e1'), e2' when List.mem i infix_ops ->
      open_hovbox 2;
      print_string "(";
      print_exp e1';
      print_space ();
      print_string i;
      print_space ();
      print_exp e2';
      print_string ")";
      close_box ()
  | _ ->
      open_hovbox 2;
      print_string "(";
      print_exp e1;
      print_break 1 2;
      print_exp e2;
      print_string ")";
      close_box ())
| AbsE abs -> print_abs abs
| LetInE (dec, exp) ->
    open_hovbox 2;
    print_string "let";
    print_space ();
    print_dec dec;
    print_space ();
    print_string "in";
    print_break 1 0;
    print_exp exp;
    close_box ()
| FixE (id, abs) ->
    open_hovbox 2;
    print_string "fix";
    print_space ();
    print_string id;
    print_space ();
    print_string "->";
    print_break 1 2;
    print_abs abs;
    close_box ()
| CondE conds ->
    open_hovbox 0;
    let print_cond s (g, b) =
      open_hovbox 2;
      print_string s;
      print_space ();
      print_exp g;
      print_space ();
      print_string "then";
      print_break 1 2;
      print_exp b;
      close_box ()
    in
    let first_cond = List.hd conds in
    let rest_conds = List.tl conds in

    let rec print_conds = function
    | [] -> ()
    | f :: r ->
        print_cond "elseif" f;
        print_break 1 1;
        print_conds r
    in
    print_cond "if" first_cond;
    print_space ();
    print_conds rest_conds;
    close_box ()
| RefE exp ->
    open_hovbox 2;
    print_string "ref";
    print_space ();
    print_exp exp;
    close_box ()
| DerefE exp ->
    print_string "!";
    print_exp exp
| AssignE (e1, e2) ->
    open_hovbox 2;
    print_exp e1;
    print_space ();
    print_string ":=";
    print_break 1 2;
    print_exp e2;
    close_box ();
| ValueE v -> print_value v
| BoxE exp ->
    open_hovbox 2;
    print_string "<";
    print_exp exp;
    print_string ">";
    close_box ()
| UnboxE exp ->
    open_hovbox 2;
    print_string "~(";
    print_exp exp;
    print_string ")";
    close_box ()
| RunE exp ->
    open_hovbox 2;
    print_string "run(";
    print_exp exp;
    print_string ")";
    close_box ()
| LiftE exp ->
    open_hovbox 2;
    print_string "lift(";
    print_break 0 4;
    print_exp exp;
    print_break 0 4;
    print_string ")";
    close_box ()
| RecE fields ->
    let print_field (id, exp) =
      open_hovbox 2;
      print_string id;
      print_space ();
      print_string "=";
      print_break 1 2;
      print_exp exp;
      close_box ()
    in
    open_hovbox 2;
    print_string "{";
    print_break 1 2;
    List.iter (fun f -> print_field f) fields;
    print_break 1 0;
    print_string "}";
    close_box ()
| SelectE (exp, field) ->
    print_exp exp;
    print_string ".";
    print_string field
| RecUpdE (r, id, exp) ->
    open_hovbox 2;
    print_string "{";
    print_break 1 2;
    print_exp r;
    print_break 1 2;
    print_string ":=";
    print_break 1 2;
    print_exp exp;
    print_break 1 0;
    print_string "}";
    close_box ()

and print_const = function
| CInt i -> print_int i
| CBool b -> print_bool b

and print_abs = function
| Abs (id, body) ->
    open_hovbox 2;
    print_string "(fun";
    print_space ();
    print_string id;
    print_space ();
    print_string "->";
    print_break 1 2;
    print_exp body;
    print_string ")";
    close_box ()

and print_dec = function
| Valbind (id, exp) ->
    print_string id;
    print_space ();
    print_string "=";
    print_space ();
    print_exp exp

and print_value = function
| ConstV c -> print_const c
| ClosV f -> print_funval f
| BoxV e -> print_exp (BoxE e)
| ListV vals ->
    open_hvbox 2;
    print_string "[";
    print_space ();
    List.iter (fun v -> print_value v; print_space()) vals;
    print_space ();
    print_string "]";
    close_box ()
| RecV fields ->
    let rec print_fields = function
    | [] -> ()
    | [(k,v)] ->
        open_hovbox 2;
        print_string k;
        print_space ();
        print_string "=";
        print_space ();
        print_value v;
        close_box ();
    | (k, v) :: rest ->
        open_hovbox 2;
        print_string k;
        print_space ();
        print_string "=";
        print_space ();
        print_value v;
        print_string ",";
        print_space ();
        close_box ();
    in
    open_hvbox 2;
    print_string "{";
    print_space ();
    print_fields fields;
    print_space ();
    print_string "}";
    close_box ()
| RefV v ->
    print_string "ref";
    print_space ();
    print_value !v
| UnitV -> print_string "()"

and print_funval = function
| StdFun (StdCurry (id, _))
| StdFun (StdFunction (id, _)) ->
    print_string ("<stdfun " ^ id ^ " >")
| Closure (env, id, body) -> print_abs (Abs (id, body))

let rec print_ty = function
| IntTy -> print_string "int"
| BoolTy -> print_string "bool"
| FunTy (t1, t2) ->
    open_hovbox 2;
    print_ty t1;
    print_space ();
    print_string "->";
    print_space ();
    print_ty t2;
    close_box ()
| VarTy _ as t ->
    (match norm_ty t with
    | VarTy v ->
        let (tyvarkind, _) = !v in
        (match tyvarkind with
        | NoLink s -> print_string ("'" ^ s)
        | _ -> failwith"")
    | not_var -> print_ty not_var)
| ListTy t -> print_string "["; print_ty t; print_string "]"
| RecTy t -> print_string "{"; print_ty t; print_string "}"
| RefTy t -> print_string "ref("; print_ty t; print_string ")"
| EmptyRow -> print_string "{}"
| Row r -> print_row r

and print_row row =
  let rec row_iter = function
  | id, ty, nextrow ->
      open_hovbox 2;
      print_string id;
      print_space ();
      print_string "=";
      print_space ();
      print_ty ty;
      (match nextrow with
      | EmptyRow -> print_string "}"
      | Row r ->
          print_string ";";
          print_space ();
          row_iter r
      | VarTy _ as v ->
          print_string "|";
          print_ty v
      | _ -> failwith"");
      close_box ()
  in
  open_hovbox 2;
  print_string "{";
  print_space ();
  row_iter row;
  print_space ();
  print_string "}";
  close_box ()
