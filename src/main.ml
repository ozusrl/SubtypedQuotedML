open Common
open Translate
open Eval
open Print

let inferty_and_print exp env =
  try
    let ty = Types.typ [0] env exp in
    print_endline "inferred type:";
    Format.print_flush ();
    print_ty ty;
    (*print_endline (Types.show_type ty);*)
    Format.print_flush ();
    print_endline ""
  with exc -> print_endline
    ("exception in type inference: " ^ Printexc.to_string exc)

let eval_and_print_w_err fn exp msg printer err_msg =
  try
    let v = fn exp in
    print_endline msg;
    Format.print_flush ();
    printer v;
    Format.print_flush ();
    print_endline ""
  with exc -> print_endline (err_msg ^ Printexc.to_string exc)

let rec parse_and_eval_exprs ?(repl = false) lexbuf =
  try
    let exp = Parser.main Lexer.mytoken lexbuf in
    inferty_and_print exp [Types.stdenv_tyrec];

    (* eval value and print *)
    eval_and_print_w_err
      StagedEval.run
      exp
      "evaluated with StagedEval:"
      print_value
      "error while running staged calc: ";

    print_endline "translation:";

    (* translate and print *)
    (try
      let (translation, _) = translate exp in
      inferty_and_print translation [Types.stdenv_tyrec];
      eval_and_print_w_err
        RecordEval.run
        translation
        "evaluated with RecordEval:"
        print_value
        "error while running record calc: ";
    with exc -> print_endline (Printexc.to_string exc));

    print_endline "----------------";
    (* run only one expression when in repl *)
    if not repl then parse_and_eval_exprs lexbuf

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
