(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/toy/toyLexer.mll,v 1.5 2000/02/11 16:16:35 fpottier Exp $ *)

(* This lexer is used to analyze toy language expressions. *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(* ML prefix. Initializes the table of keywords. *)

{

open ToyParser

exception Error of string * int * int

let fail lexbuf message =
  raise (Error(message, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf))

let keyword_table =
  Hashtbl.create 149

let () = List.iter (fun (string, token) ->
  Hashtbl.add keyword_table string token
) [
  "and", AND;
  "as", AS;
  "function", FUNCTION;
  "fun", FUN;
  "let", LET;
  "in", IN;
  "rec", REC;
  "if", IF;
  "then", THEN;
  "else", ELSE;
  "true", TRUE;
  "false", FALSE;
  "of", OF;
  "type", TYPE;
  "match", MATCH;
  "with", WITH;
  "begin", LPAREN;
  "end", RPAREN;
  "try", TRY;
  "value", VALUE;
  "ref", REF;
]

(* The following code comes straight from O'Caml's code. *)

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To translate escape sequences. *)

let char_for_backslash =
      begin function
      | 'n' -> '\010'
      | 'r' -> '\013'
      | 'b' -> '\008'
      | 't' -> '\009'
      | c   -> c
      end

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in  
  Char.chr(c land 0xFF)

(* To store the position of the beginning of a string or comment. *)

let start_pos = ref 0

}

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Rules. *)

rule token = parse
    [ ' ' '\010' '\013' '\009' '\012' ] +
            { token lexbuf }
  | [ 'a'-'z' '\224'-'\246' '\248'-'\254' ]
    [ 'A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\254'
      '0'-'9' '\'' '_' '<' '>' '=' '+' '-' ':' '&' '|' '@' '!' '*' '/' '^' ] *
            { let s = Lexing.lexeme lexbuf in
              try Hashtbl.find keyword_table s
              with Not_found -> IDENT s }
  | [ 'A'-'Z' '\192'-'\214' ]
    [ 'A'-'Z' 'a'-'z' '\192'-'\214' '\216'-'\246' '\248'-'\254'
      '0'-'9' '\'' '_' ] *
            { let s = Lexing.lexeme lexbuf in
	      try Hashtbl.find keyword_table s
	      with Not_found -> CONSTRUCTOR (Lexing.lexeme lexbuf) }
  | '-'? ['0'-'9']+
    | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    | '0' ['o' 'O'] ['0'-'7']+
    | '0' ['b' 'B'] ['0'-'1']+
            { INTCONST (int_of_string (Lexing.lexeme lexbuf)) }
  | '-'? ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOATCONST (float_of_string(Lexing.lexeme lexbuf)) }
  | "\""
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        start_pos := string_start;
        string lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        STRINGCONST (get_stored_string()) }
  | "'" [^ '\\' '\''] "'"
      { CHARCONST(Lexing.lexeme_char lexbuf 1) }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { CHARCONST(char_for_backslash (Lexing.lexeme_char lexbuf 2)) }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { CHARCONST(char_for_decimal_code lexbuf 2) }
  | "(*"
            { comment lexbuf; token lexbuf }

  | "<-"    { LEFTARROW }
  | "!"	    { BANG }
  | "|]"    { BARRBRACKET }
  | "|"	    { BAR }  
  | ","	    { COMMA }
  | "."     { DOT }
  | "()"    { UNIT }
  | "("     { LPAREN }
  | ")"     { RPAREN }
  | "[|"    { LBRACKETBAR }
  | "{"	    { LBRACE }
  | "}"     { RBRACE }
  | "->"    { ARROW }
  | "="     { EQUAL }
  | ";"	    { SEMI }
  | ";;"    { SEMISEMI }
  | ":="    { ASSIGN }
  | ":"     { COLON }
  | "_"	    { UNDERSCORE }
  | "#include" { INCLUDE }
  | "#"	    { SHARP }
  | "@@"    { ATAT }
  | "@"     { AT }
  | "\\"    { SETMINUS }
  | eof     { raise End_of_file }

  | _       { fail lexbuf "Illegal character" }

and comment = parse
    "*)"
            { () }
  | "(*"
            { comment lexbuf; comment lexbuf }
  | eof
            { fail lexbuf "Unterminated comment" }
  | _
            { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { fail lexbuf "Unterminated string" }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

