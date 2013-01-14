open Common

let _ =
  while true do
    let lexbuf = Lexing.from_channel stdin in
    print_string "> "; flush stdout;
    try
        let exp = Parser.main Lexer.mytoken lexbuf in
        Printf.printf "Parsed expr: %s\n" (show_exp exp);
        let vl = Eval.eval Eval.stdenv exp in
        print_endline (show_val vl)
    with Parsing.Parse_error -> print_endline "Parse error.\n"
       | Failure s -> print_endline ("Unexpected error occured: " ^ s)
       | Lexer.EndInput -> exit 0
       | Eval.TypeMismatch (expected, found) ->
           Printf.printf "TypeMismatch: expected: %s, found: %s.\n" expected found
       | exc       -> print_endline (Printexc.to_string exc)
  done
