{
  open Parser
  exception EndInput
}

let numeric      = ['0' - '9']
let letter       = ['a' - 'z' 'A' - 'Z']
let quote        = ['~']
let numeric_plus = numeric+
let numeric_star = numeric*

rule mytoken = parse
  | [' ' '\t' '\n'] { mytoken lexbuf }  (* skip over whitespace *)
  | "="        { EQ }
  | "("        { LP }
  | ")"        { RP }
  | "["        { LBRACK }
  | "]"        { RBRACK }
  | "<"        { LT }
  | ">"        { GT }
  | "."        { DOT }
  | ";"        { SEMI }
  | "fun"      { FUN }
  | "~"        { UNBOX }
  | "run"      { RUN }
  | "in"       { IN }
  | "let"      { LET }
  | "rec"      { REC }
  | "true"     { BOOL(true) }
  | "false"    { BOOL(false) }
  | "{"        { LB }
  | "}"        { RB }
  | ","        { COMMA }
  | "->"       { ARROW }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "*"        { MULT }
  | "/"        { DIV }
  | "::"       { CONS }
  | "if"       { IF }
  | "else"     { ELSE }
  | "then"     { THEN }
  | "lift"     { LIFT }
  | "fix"      { FIX }

  | (letter (letter | numeric | '_')*) as id   { ID id }
  | numeric_plus as i  { INT (int_of_string i) }
  | eof                { raise EndInput  }

(* do not modify this function: *)
{
   let lextest s =
    let rec lbuf = Lexing.from_string s
    and lextest_aux () =
      try
        let res = mytoken lbuf in
        res :: lextest_aux ()
      with EndInput -> []
    in
      lextest_aux ()
}
