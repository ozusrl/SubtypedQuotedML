open Common
open Translate
open Eval
open Print

let rec parse_and_eval_exprs ?(repl = false) lexbuf =
  try
    let exp = Parser.main Lexer.mytoken lexbuf in
    (*Printf.printf "Parsed expr: %s\n" (show_exp exp);*)

    try
      (* infer type and print *)
      (*let exp_ty = Types.fresh_tyvar () in*)
      (*let subs   = Types.infer Types.test_env exp exp_ty in*)
      (*let ty     = Types.apply_sub_ty subs exp_ty in*)
      (*Printf.printf "Type without substitution: %s\n" (Types.show_type exp_ty);*)
      (*Printf.printf "Type: %s\n" (Types.show_type ty);*)

      (try
        let ty2 = Types2.typ 0 Types2.stdenv exp in
        print_endline "Type2:";
        Format.print_flush ();
        print_ty ty2;
        Format.print_flush ();
        print_endline "";
      with exc -> print_endline
        ("exception in types2: " ^ Printexc.to_string exc));

      (* translate and print *)
      let translation =
        try
          let (translation, _) = translate exp in
          Some translation
        with exc -> print_endline (Printexc.to_string exc); None
      in

      (* print type of translation *)
      (match translation with
      | Some t ->
        (try
          let ty2 = Types2.typ 0 Types2.stdenv t in
          print_endline "type of translation:";
          Format.print_flush ();
          print_ty ty2;
          Format.print_flush ();
          print_endline "";
        with exc -> print_endline
          ("exception in types2: " ^ Printexc.to_string exc))
      | _ -> ());

      (* eval value and print *)
      (try
        let value = StagedEval.run exp in
        print_endline "Return value in staged calc:";
        Format.print_flush ();
        print_value value;
        Format.print_flush ();
        print_endline "";
      with exc -> print_endline
        ("error while running staged calc: " ^ Printexc.to_string exc));

      (try
        let value = RecordEval.run exp in
        print_endline "Return value in record calc:";
        Format.print_flush ();
        print_value value;
        Format.print_flush ();
        print_endline "";
      with exc -> print_endline
        ("error while running staged calc: " ^ Printexc.to_string exc));

      (match translation with
      | None -> ()
      | Some t ->
        (*Printf.printf "Translation: %s\n" (show_exp t);*)
        try
          let value2 = RecordEval.run t in
          print_endline "Return value of translation in record calc:";
          Format.print_flush ();
          print_value value2;
          Format.print_flush ();
          print_endline "";
        with TypeMismatch (expected, found) ->
               Printf.printf "TypeMismatch: expected: %s, found: %s.\n" expected found
           | exc ->
               print_endline ("error while running record calc: " ^ (Printexc.to_string exc)));

      print_endline "";
      (* run only one expression when in repl *)
      if not repl then parse_and_eval_exprs lexbuf
    with Failure s -> print_endline ("Unexpected error occured: " ^ s)
       | TypeMismatch (expected, found) ->
           Printf.printf "TypeMismatch: expected: %s, found: %s.\n" expected found
       | exc -> print_endline (Printexc.to_string exc)

  with Parsing.Parse_error -> print_endline "Parse error."
     | Lexer.EndInput -> ()
     | exc -> print_endline (Printexc.to_string exc)

let _ =
  if Array.length (Sys.argv) < 2 then
    while true do
      let lexbuf = Lexing.from_channel stdin in
      print_string "> "; flush stdout;
      parse_and_eval_exprs ~repl:true lexbuf
    done
  else
    let input_file = Sys.argv.(1) in
    let lexbuf = Lexing.from_channel (open_in input_file) in
    parse_and_eval_exprs lexbuf
