open Common
open Eval

let rec parse_and_eval_exprs ?(repl = false) lexbuf = 
  try
    let exp = Parser.main Lexer.mytoken lexbuf in
    Printf.printf "Parsed expr: %s\n\n" (show_exp exp);

    try
      (* infer type and print *)
      (*let exp_ty = Types.fresh_tyvar () in*)
      (*let subs = Types.infer Types.test_env exp exp_ty in*)
      (*let ty = Types.apply_sub_ty subs exp_ty in*)
      (*Printf.printf "Type without substitution: %s\n" (Types.show_type exp_ty);*)
      (*Printf.printf "Type: %s\n" (Types.show_type ty);*)

      (* eval value and print *)
      let value = Eval2.eval stdenv exp in
      Printf.printf "Return value: %s\n\n\n" (show_val value);

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
