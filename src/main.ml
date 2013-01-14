open Common

let _ =
  try
    while true do
      let lexbuf = Lexing.from_channel stdin in
      print_string "> "; flush stdout;
      try
          let exp = Parser.main Lexer.mytoken lexbuf in
          (*Printf.printf "Parsed expr: %s\n" (show_exp exp);*)
          let vl = Eval.eval exp in
          print_endline (show_val vl)
      with Parsing.Parse_error -> print_endline "Parse error.\n"
         | Failure _ -> print_endline "Unexpected error occured (lexer error?)"
    done
  with Lexer.EndInput -> exit 0
