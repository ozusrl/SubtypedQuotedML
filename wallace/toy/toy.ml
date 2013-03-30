(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/toy/toy.ml,v 1.15 2000/02/11 16:16:34 fpottier Exp $ *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Setting up the modules} *)

module Map =
  SetBasedMap.Make(Baltree.Weight) (* TEMPORARY try Baltree.Height, ListSet *)

module Env =
  SimpleEnv.Make(Map) (* TEMPORARY try UnorderedListMap *)

module OrderedInt = struct
  type t = int
  let compare = (-)
end

module IntMap =
  FixMap.Make(Map)(OrderedInt)

module Context = (* TEMPORARY try Patricia *)
  IntMap

module Engine =
  Engine.Make(Env)(Context)

open Engine

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Trying things out} *)

let handle_phrase env = function
  | ToySyntax.PhraseExpr e ->
      let e = Compile.expression e in
      (*InternalSyntax.print e; flush stdout; print_newline(); print_newline();*)
      let scheme = simplify (infer env e) in
      Print.text true scheme;
      print_newline();
      flush stdout;
      None
  | ToySyntax.PhraseLet (fix, bindings) ->
      let open ToySyntax in

      (match bindings with
      | [PVar var, exp] ->
	  let e = Compile.expression exp in
	  let scheme = simplify (infer env e) in
	  Print.text true scheme;
	  print_newline();
	  flush stdout;
	  Some (Engine.add_to_env var scheme env)
      | _ -> failwith "unsupprted let declaration")

  | _ ->
      failwith "Other phrases currently unsupported." (* TEMPORARY *)

let failure message =
  Printf.printf "%s.\n\n" message;
  flush stdout

let handle_channel channel =
  let lexbuf = Lexing.from_channel channel in

  let rec iter env =
    try
      print_string "? "; flush stdout;
      let phrase = ToyParser.phrase ToyLexer.token lexbuf in
      print_newline();
      (match handle_phrase env phrase with
      | None -> iter env
      | Some env' -> iter env')
    with
    | ToyLexer.Error (message, start_loc, end_loc) ->
	failure (Printf.sprintf
	  "%s at characters %d-%d" message start_loc end_loc)
    | Parsing.Parse_error ->
	failure "Parse error"
    | ToyParserUtil.DuplicateLabel label ->
	failure (Printf.sprintf
	  "Label `%s' appears twice in some record pattern" label)
    | Compile.HeterogeneousMatching ->
	failure "Heterogeneous matching"
    | Compile.NonRectangularMatching ->
	failure "Non-rectangular matching"
    | Compile.IllegalPattern message ->
	failure message
    | Engine.Failure message ->
	failure message
    | Core.Closure.Inconsistent ->
	failure "Constraint inconsistency"
    | Label.Integer.Clash (string1, string2) ->
	failure (Printf.sprintf
		   "Labels ``%s'' and ``%s'' map to the same 31-bit tag"
		   string1 string2)
    | Sys_error message ->
	failure ("System error: " ^ message)
  in
  iter Engine.builtin

(*let () = handle_channel stdin*)

(*i(* Here is some test code. *)

open ToySyntax

let identity =
  EFun [[PVar "x"], EVar "x"]

(* let f0 x = x in
   let f1 x = (f0 x) @ { x1 = () } in
   let f2 x = (f1 x) @ { x2 = () } in
   let f3 x = (f2 x) @ { x3 = () } in
   ... in
   fn {} *)

let label n =
  Printf.sprintf "x%d" n

let rec test1 n =
  if n = 0 then
    identity
  else
    EFun [[PVar "x"], EApp(EApp(EVar "@", EApp(test1 (n-1), EVar "x")), 
			                           ERecord [label n, EVar "_unit"])]
let test1 n =
  EApp(test1 n, ERecord [])

let rec test2 n =
  if n = 0 then
    identity
  else
    EFun [[PVar "x"], EApp(EApp(EVar "@", ERecord [label n, EVar "_unit"]), 
			                           EApp(test2 (n-1), EVar "x"))]
let test2 n =
  EApp(test2 n, ERecord [])

(* Final test code. *)

let run e =
  let e = Compile.expression e in
  let t0 = Sys.time() in
  let scheme = infer Engine.builtin e in
  let t1 = Sys.time() in
  let scheme = simplify scheme in
  let t2 = Sys.time() in
  Print.text true scheme;
  print_newline();
  Printf.printf "\nInference took %f seconds, simplification took %f seconds.\n"
    (t1 -. t0) (t2 -. t1);
  flush stdout

let _ =
  run (test1 30)

i*)
